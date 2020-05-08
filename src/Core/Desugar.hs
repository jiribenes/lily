{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Desugar
  ( desugarTopLevelFunction
  , desugarTopLevel
  )
where

import           Core.Syntax
import           Core.Located
import           Language.C.Clang.Cursor
import           Control.Monad.Except           ( throwError
                                                , MonadError
                                                )
import qualified Language.C.Clang.Cursor.Typed as T
import           Data.Maybe                     ( fromJust )
import           Clang
import           ClangType
import           Error
import           Type                           ( nameFromBS
                                                , Name(..)
                                                )
import           Data.Foldable                  ( foldlM )
import           Control.Lens
import           Debug.Trace                    ( traceShowM
                                                , traceM
                                                )
import qualified Data.Graph                    as G
import           Data.Text.Prettyprint.Doc      ( Pretty(pretty) )

data DesugarError = WeirdFunctionBody
                  | UnknownBinaryOperation
                  | UnknownUnaryOperation
                  | UnknownDeclarationKindInBlock
                  | BlockExpected Location
                  | InvalidIfShape
                  | DesugarWeirdness
    deriving stock (Show, Eq)

desugarExpr :: MonadError DesugarError m => Cursor -> m Expr
desugarExpr cursor = case cursorKind cursor of
  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    binOp     <- note UnknownBinaryOperation
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)
    let binOpBuiltin = Builtin cursor $ BuiltinBinOp binOp
    pure $ App cursor (App cursor binOpBuiltin leftExpr) rightExpr

  UnaryOperator -> do
    let [child] = cursorChildren cursor

    childExpr <- desugarExpr child

    unOp      <- note UnknownUnaryOperation
      $ parseUnOp (fromJust $ T.matchKind @ 'UnaryOperator $ cursor)
    let unOpBuiltin = Builtin cursor $ BuiltinUnOp unOp

    pure $ App cursor unOpBuiltin childExpr

  DeclRefExpr -> do
    let name = nameFromBS $ cursorSpelling cursor
    pure $ Var cursor name

  CXXNewExpr -> do
    child     <- note DesugarWeirdness $ cursorChildren cursor ^? _last
    typ       <- note DesugarWeirdness $ fromClangType =<< cursorType cursor

    childExpr <- desugarExpr child

    pure $ App cursor (Builtin cursor (BuiltinNew typ)) childExpr

  ArraySubscriptExpr -> do
    let [left, right] = cursorChildren cursor

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    let subscriptBuiltin = Builtin cursor BuiltinArraySubscript
    pure $ App cursor (App cursor subscriptBuiltin leftExpr) rightExpr

  CallExpr -> do
    -- TODO: handle single argument function calls!
    exprs <- traverse desugarExpr $ cursorChildren cursor

    pure $ foldl1 (App cursor) exprs

  MemberRefExpr -> do
    case cursorChildren cursor of
      [child] -> do
        expr <- desugarExpr child

        let memberName = nameFromBS $ cursorSpelling cursor
        let member     = Var cursor memberName

        let memberRef  = Builtin cursor BuiltinMemberRef

        pure $ App cursor (App cursor memberRef expr) member
      [] -> do
        traceM
          "I don't support weird member reference for C++ classes yet! TODO"
        throwError DesugarWeirdness
      _ -> throwError DesugarWeirdness

  IntegerLiteral        -> pure $ Literal cursor
  CharacterLiteral      -> pure $ Literal cursor
  CXXBoolLiteralExpr    -> pure $ Literal cursor
  CXXNullPtrLiteralExpr -> pure $ Builtin cursor BuiltinNullPtr

  FirstExpr             -> desugarSingleChild cursor
  other                 -> error $ "found: " <> show other

desugarBlock :: MonadError DesugarError m => T.CursorK 'CompoundStmt -> m Expr
desugarBlock cursor = do
  result <- longFunc
  pure $ result $ unit (T.withoutKind cursor)
 where
  go
    :: MonadError DesugarError m => (Expr -> Expr) -> Cursor -> m (Expr -> Expr)
  go cont c = do
    resultFn <- desugarBlockOne c
    pure $ cont . resultFn

  longFunc :: MonadError DesugarError m => m (Expr -> Expr)
  longFunc = foldlM go id $ T.cursorChildren cursor

desugarStmt :: MonadError DesugarError m => Cursor -> m Expr
desugarStmt cursor = case cursorKind cursor of
  CompoundStmt ->
    desugarBlock $ fromJust $ T.matchKind @ 'CompoundStmt $ cursor
  _ -> do
    stmtCont <- desugarBlockOne cursor
    pure $ stmtCont $ unit cursor

desugarBlockOne :: MonadError DesugarError m => Cursor -> m (Expr -> Expr)
desugarBlockOne cursor = case cursorKind cursor of
  DeclStmt -> do
    let [child] = cursorChildren cursor

    case cursorKind child of
      VarDecl -> do
        let [grandchild] = cursorChildren child

        expr <- desugarExpr grandchild

        let name = nameFromBS $ cursorSpelling child

        pure $ \rest -> LetIn child name expr rest
      _ -> throwError UnknownDeclarationKindInBlock

  ReturnStmt -> do
    let [child] = cursorChildren cursor

    expr <- desugarExpr child

    pure $ \_ -> expr

  IfStmt -> do
    case cursorChildren cursor of
      [cond, thn] -> do
        condition <- desugarExpr cond
        expr      <- desugarStmt thn

        pure $ \els -> If cursor condition expr els

      [cond, thn, els] -> do
        condition <- desugarExpr cond

        expr1     <- desugarStmt thn
        expr2     <- desugarStmt els

        pure $ \_ -> If cursor condition expr1 expr2

      _ -> throwError InvalidIfShape

  other -> do
    traceM $ "Encountered: " <> show other <> " in a block. Hopefully it's ok!"
    traceM "I'll interpret it as a normal expression!"
    -- it's not a declaration
    -- so rewrite it as `let _ = <expr> in...`

    expr <- desugarExpr cursor

    -- TODO: make throwaway name!
    pure $ \rest -> LetIn cursor (Name "_") expr rest

desugarSingleChild :: MonadError DesugarError m => Cursor -> m Expr
desugarSingleChild cursor = do
  let [child] = cursorChildren cursor
  desugarExpr child

desugarTopLevelFunction
  :: MonadError DesugarError m => SomeFunctionCursor -> m TopLevel
desugarTopLevelFunction cursor = do
  -- these are typically the first children but I really want to be safe here 
  let parameterF :: Fold Cursor (T.CursorK 'ParmDecl)
      parameterF = cursorChildrenF . folding (T.matchKind @ 'ParmDecl)

  -- this is typically the last child but again, trying to be safe here!
  let bodyF :: Fold Cursor (T.CursorK 'CompoundStmt)
      bodyF = cursorChildrenF . folding (T.matchKind @ 'CompoundStmt)

  let untypedCursor = unwrapSomeFunction cursor
  let parameters    = untypedCursor ^.. parameterF
  let name          = nameFromBS $ cursorSpelling untypedCursor

  case untypedCursor ^.. bodyF of
    [body] -> do

      functionHeader <- desugarParameters parameters
      functionBody   <- desugarBlock body

      let function = functionHeader functionBody

      pure $ TLLet $ Let untypedCursor name function
    [] -> pure $ TLLetNoBody untypedCursor name
    _  -> throwError WeirdFunctionBody

desugarParameters
  :: MonadError DesugarError m => [T.CursorK 'ParmDecl] -> m (Expr -> Expr)
desugarParameters = foldlM go id
 where
  go
    :: MonadError DesugarError m
    => (Expr -> Expr)
    -> T.CursorK 'ParmDecl
    -> m (Expr -> Expr)
  go cont cursor = do
    let name      = nameFromBS $ cursorSpelling $ T.withoutKind cursor
    let clangType = Just $ T.cursorType cursor
    let lam       = Lam (T.withoutKind cursor) clangType name

    pure $ cont . lam

desugarSCCTopLevel
  :: MonadError DesugarError m => G.SCC SomeFunctionCursor -> m TopLevel
desugarSCCTopLevel (G.AcyclicSCC cursor ) = desugarTopLevelFunction cursor
desugarSCCTopLevel (G.CyclicSCC  cursors) = do
  topLevelFunctions <- traverse desugarTopLevelFunction cursors
  let properFunctions = topLevelFunctions ^.. each . folding (preview _TLLet)
  properFunctions & TLLetRecursive & pure

desugarTopLevel :: [G.SCC SomeFunctionCursor] -> Either DesugarError [TopLevel]
desugarTopLevel = traverse desugarSCCTopLevel

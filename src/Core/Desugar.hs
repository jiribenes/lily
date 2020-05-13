{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Lens
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.Reader           ( runReaderT
                                                , MonadReader
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Graph                    as G
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Debug.Trace                    ( traceM )
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

import           Clang.Function
import           Clang.Type
import           Clang.OpParser
import           Clang.Struct                   ( StructCursor )
import           Core.Located
import           Core.Syntax
import           Error
import           Name                           ( Name(Name)
                                                , nameFromBS
                                                )
import           Type.Type                      ( Kind(..)
                                                , TCon(..)
                                                , Type(..)
                                                )


data DesugarError = WeirdFunctionBody
                  | UnknownBinaryOperation
                  | UnknownUnaryOperation
                  | UnknownDeclarationKindInBlock
                  | BlockExpected Location
                  | InvalidIfShape
                  | ExpectedValidType
                  | NotImplementedYet
                  | DesugarWeirdness -- this is a catch-all TODO: replace by appropriate use!
    deriving stock (Show, Eq)

newtype DesugarEnv = DesugarEnv { _allStructs :: [Struct] }
  deriving stock (Eq, Show)
  deriving newtype (Monoid, Semigroup)

type MonadDesugar m = (MonadError DesugarError m, MonadReader DesugarEnv m)

desugarExpr :: MonadDesugar m => Cursor -> m Expr
desugarExpr cursor = case cursorKind cursor of
  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    resultTyp <- extractType cursor
    opTyp     <- extractType left

    binOp     <- note UnknownBinaryOperation
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)
    let binOpBuiltin = Builtin cursor $ BuiltinBinOp binOp resultTyp opTyp
    pure $ App cursor (App cursor binOpBuiltin leftExpr) rightExpr

  UnaryOperator -> do
    let [child] = cursorChildren cursor

    childExpr <- desugarExpr child

    resultTyp <- extractType cursor
    opTyp     <- extractType child

    unOp      <- note UnknownUnaryOperation
      $ parseUnOp (fromJust $ T.matchKind @ 'UnaryOperator $ cursor)
    let unOpBuiltin = Builtin cursor $ BuiltinUnOp unOp resultTyp opTyp

    pure $ App cursor unOpBuiltin childExpr

  DeclRefExpr -> do
    let name = nameFromBS $ cursorSpelling cursor
    pure $ Var cursor name

  CXXNewExpr -> do
    -- TODO: This is not correct at all!
    -- look at 'new int' vs 'new int[7]' vs 'new Struct' vs 'new Struct[10]'
    case cursorChildren cursor of
      [] -> do
        typ <- extractType cursor
        pure $ Builtin cursor (BuiltinNew typ)
      xs -> do
        typ       <- extractType cursor
        child     <- note DesugarWeirdness $ xs ^? _last

        childExpr <- desugarExpr child

        pure $ App cursor (Builtin cursor (BuiltinNewArray typ)) childExpr

  ArraySubscriptExpr -> do
    let [left, right] = cursorChildren cursor

    leftExpr     <- desugarExpr left
    rightExpr    <- desugarExpr right

    arrayTyp     <- extractType left
    subscriptTyp <- extractType right

    let subscriptBuiltin =
          Builtin cursor $ BuiltinArraySubscript arrayTyp subscriptTyp
    pure $ App cursor (App cursor subscriptBuiltin leftExpr) rightExpr

  CallExpr -> do
    exprs <- traverse desugarExpr $ cursorChildren cursor

    -- TODO: can we write this using Lens?
    let exprs' = case exprs of
          [] -> [unit cursor]
          xs -> xs

    pure $ foldl1 (App cursor) exprs'

    {- TODO: this is needed for constructors

      CallExpr -> do
    let (fn : args) = cursorChildren cursor
    exprs  <- traverse desugarExpr args

    fnExpr <- case cursorKind fn of
      DeclRefExpr -> desugarExpr fn
      _           -> do
        let Just ctor =
              fn
                ^? cursorDescendantsF
                .   folding (T.matchKind @ 'CallExpr)
                .   to T.withoutKind
        referenced <- note DesugarWeirdness $ cursorReferenced ctor
        case cursorKind referenced of
          StructDecl -> do
            traceShowM "found struct!"
            traceShowM $ cursorUSR referenced
            throwError DesugarWeirdness
          other -> do
            traceShowM $ "found " <> show other
            traceShowM $ cursorSpelling referenced
            traceShowM $ cursorUSR referenced
            throwError DesugarWeirdness

      other -> do
        traceShowM other
        throwError DesugarWeirdness


    -- TODO: can we write this using Lens?
    let exprs' = case exprs of
          [] -> [unit cursor]
          xs -> xs

    pure $ foldl (App cursor) fnExpr exprs'
    -}

  MemberRefExpr -> case cursorChildren cursor of
    [child] -> do
      expr <- desugarExpr child

      let memberName = nameFromBS $ cursorSpelling cursor
      let member     = Var cursor memberName

      let memberRef  = Builtin cursor BuiltinMemberRef

      pure $ App cursor (App cursor memberRef expr) member
    [] -> do
      traceM "I don't support weird member reference for C++ classes yet! TODO"
      throwError NotImplementedYet
    _ -> throwError DesugarWeirdness

  IntegerLiteral        -> desugarLiteral cursor
  CharacterLiteral      -> desugarLiteral cursor
  CXXBoolLiteralExpr    -> desugarLiteral cursor
  CXXNullPtrLiteralExpr -> pure $ Builtin cursor BuiltinNullPtr

  FirstExpr             -> desugarSingleChild cursor
  other                 -> error $ "found: " <> show other

desugarLiteral :: MonadDesugar m => Cursor -> m Expr
desugarLiteral cursor = do
  typ <- extractType cursor
  pure $ Literal cursor typ

desugarBlock :: MonadDesugar m => T.CursorK 'CompoundStmt -> m Expr
desugarBlock cursor = do
  result <- longFunc
  pure $ result $ unit (T.withoutKind cursor)
 where
  go :: MonadDesugar m => (Expr -> Expr) -> Cursor -> m (Expr -> Expr)
  go cont c = do
    resultFn <- desugarBlockOne c
    pure $ cont . resultFn

  longFunc :: MonadDesugar m => m (Expr -> Expr)
  longFunc = foldlM go id $ T.cursorChildren cursor

desugarStmt :: MonadDesugar m => Cursor -> m Expr
desugarStmt cursor = case cursorKind cursor of
  CompoundStmt ->
    desugarBlock $ fromJust $ T.matchKind @ 'CompoundStmt $ cursor
  _ -> do
    stmtCont <- desugarBlockOne cursor
    pure $ stmtCont $ unit cursor

desugarBlockOne :: MonadDesugar m => Cursor -> m (Expr -> Expr)
desugarBlockOne cursor = case cursorKind cursor of
  DeclStmt -> do
    let [child] = cursorChildren cursor

    case cursorKind child of
      VarDecl -> do
        grandchild <- note DesugarWeirdness $ cursorChildren child ^? _last

        expr       <- desugarExpr grandchild

        let name = nameFromBS $ cursorSpelling child

        pure $ \rest -> LetIn child name expr rest
      _ -> throwError UnknownDeclarationKindInBlock

  ReturnStmt -> case cursorChildren cursor of
    [child] -> do
      expr <- desugarExpr child

      pure $ \_ -> expr
    [] -> pure $ \_ -> unit cursor
    _  -> throwError DesugarWeirdness

  IfStmt -> case cursorChildren cursor of
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

  CallExpr -> do
      -- TODO: should we force here that this has a void return value?
      --       what to do otherwise with ignored return values?
    expr <- desugarExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  FirstExpr -> do
      -- TODO: should we force here that this has a void return value?
      --       what to do otherwise with ignored return values?
    expr <- desugarExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    binOp <- note UnknownBinaryOperation
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)

    unless (isAssignOp binOp) $ throwError DesugarWeirdness
    unless (not $ isJust $ withoutAssign binOp)
      $ error "+= etc. not supported yet"

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    let setter =
          App cursor (App cursor (Builtin cursor BuiltinSet) leftExpr) rightExpr

    pure $ \rest -> LetIn cursor (Name "_") setter rest

  other -> do
    -- best effort 
    traceM $ "Encountered: " <> show other <> " in a block. Hopefully it's ok!"
    traceM "I'll interpret it as a normal expression!"
    -- it's not a declaration
    -- so rewrite it as `let _ = <expr> in...`

    expr <- desugarExpr cursor

    -- TODO: make throwaway name!
    pure $ \rest -> LetIn cursor (Name "_") expr rest

desugarSingleChild :: MonadDesugar m => Cursor -> m Expr
desugarSingleChild cursor = do
  let [child] = cursorChildren cursor
  desugarExpr child

desugarTopLevelFunction
  :: MonadReader DesugarEnv m
  => MonadDesugar m => SomeFunctionCursor -> m TopLevel
desugarTopLevelFunction = \case
  SomeConstructor c -> desugarConstructor c
  cursor            -> do
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
      _ -> throwError WeirdFunctionBody

-- TODO: Add constructor specialties if needed
desugarConstructor :: MonadDesugar m => ConstructorCursor -> m TopLevel
desugarConstructor cursor = do
  -- these are typically the first children but I really want to be safe here 
  let parameterF :: Fold Cursor (T.CursorK 'ParmDecl)
      parameterF = cursorChildrenF . folding (T.matchKind @ 'ParmDecl)

  -- this is typically the last child but again, trying to be safe here!
  let bodyF :: Fold Cursor (T.CursorK 'CompoundStmt)
      bodyF = cursorChildrenF . folding (T.matchKind @ 'CompoundStmt)

  let untypedCursor = T.withoutKind cursor
  let parameters    = untypedCursor ^.. parameterF
  let name          = nameFromBS $ cursorSpelling untypedCursor

  case untypedCursor ^.. bodyF of
    [body] -> do

      functionHeader <- desugarParameters parameters
      functionBody   <- desugarBlock body

      let function = functionHeader functionBody

      pure $ TLLet $ Let untypedCursor name function
    _ -> throwError WeirdFunctionBody

desugarParameters
  :: MonadReader DesugarEnv m
  => MonadDesugar m => [T.CursorK 'ParmDecl] -> m (Expr -> Expr)
desugarParameters = foldlM go id
 where
  go
    :: MonadDesugar m
    => (Expr -> Expr)
    -> T.CursorK 'ParmDecl
    -> m (Expr -> Expr)
  go cont cursor = do
    let name = nameFromBS $ cursorSpelling $ T.withoutKind cursor
    let lam  = Lam (T.withoutKind cursor) name

    pure $ cont . lam

desugarSCCTopLevel
  :: MonadReader DesugarEnv m
  => MonadDesugar m => G.SCC SomeFunctionCursor -> m TopLevel
desugarSCCTopLevel (G.AcyclicSCC cursor ) = desugarTopLevelFunction cursor
desugarSCCTopLevel (G.CyclicSCC  cursors) = do
  topLevelFunctions <- traverse desugarTopLevelFunction cursors
  let properFunctions = topLevelFunctions ^.. each . folding (preview _TLLet)
  case properFunctions of
    [] -> throwError DesugarWeirdness
    xs -> xs & NE.fromList & TLLetRecursive & pure

desugarTopLevel
  :: [StructCursor]
  -> [G.SCC SomeFunctionCursor]
  -> Either DesugarError [TopLevel]
desugarTopLevel structs fnSccs = do
  desugaredStructs <- traverse desugarStruct structs
  let desugarEnv = DesugarEnv desugaredStructs

  tlFns <- traverse (flip runReaderT desugarEnv . desugarSCCTopLevel) fnSccs
  pure $ (TLStruct <$> desugaredStructs) <> tlFns

desugarStruct :: MonadError DesugarError m => StructCursor -> m Struct
desugarStruct cursor = do
  let fields =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'FieldDecl)
  let name = nameFromBS $ T.cursorSpelling cursor

  desugaredFields <- traverse desugarField fields

  let tcon   = TCon $ TC name StarKind

  let struct = Struct (T.withoutKind cursor) tcon name desugaredFields
  pure struct

desugarField
  :: MonadError DesugarError m => T.CursorK 'FieldDecl -> m StructField
desugarField cursor = do
  let untypedCursor = T.withoutKind cursor
  let name          = nameFromBS $ T.cursorSpelling cursor
  typ <- extractType untypedCursor

  pure $ StructField untypedCursor typ name

extractType :: MonadError DesugarError m => Cursor -> m Type
extractType c = note ExpectedValidType $ fromClangType =<< cursorType c

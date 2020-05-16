{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Debug.Trace                    ( traceShowM
                                                , traceM
                                                )
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
import           Data.List                      ( find )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

data DesugarError = WeirdFunctionBody Location
                  | UnknownBinaryOperation Location
                  | UnknownUnaryOperation Location
                  | UnknownDeclarationKindInBlock Location
                  | InvalidIfShape Location
                  | ExpectedValidType Location
                  | NotImplementedYet Location
                  | ExpectedValidStruct Location
                  | WeirdNewOperator Location
                  | ExpectedReferencedExpr Location
                  | ExpectedThis Location
                  | DesugarWeirdness Location -- this is a catch-all TODO: replace by appropriate use!
    deriving stock (Show, Eq)

instance Pretty DesugarError where
  pretty = \case
    WeirdFunctionBody l -> PP.align
      (PP.sep
        [ "Function body has a weird shape! Expected exactly one 'CompoundStmt'!"
        , "Location: " <+> prettyLocation l
        ]
      )
    UnknownBinaryOperation l ->
      "Unknown binary operation at: " <+> prettyLocation l
    UnknownUnaryOperation l ->
      "Unknown unary operation at: " <+> prettyLocation l
    UnknownDeclarationKindInBlock l ->
      "Unknown declaration kind in block at: " <+> prettyLocation l
    InvalidIfShape l -> PP.align
      (PP.sep
        [ "If statement has a weird shape! Expected: " <+> PP.align
          (PP.sep
            [ "EITHER `if <something> then <something>`"
            , "    OR `if <something> then <something> else <something>`"
            ]
          )
        , "Location: " <+> prettyLocation l
        ]
      )
    ExpectedValidType l ->
      "Expected a valid Clang type for expression at: " <+> prettyLocation l
    NotImplementedYet l -> PP.align
      (PP.sep
        [ "TODO: Desugaring of this AST node is not implemented yet!"
        , "Location: " <+> prettyLocation l
        ]
      )
    ExpectedValidStruct l -> "Invalid struct at: " <+> prettyLocation l
    WeirdNewOperator l ->
      "Invalid form of the `new` operator at: " <+> prettyLocation l
    ExpectedReferencedExpr l -> PP.align
      (PP.sep
        [ "Expected that expression is referencing another (possibly a function call?)"
        , "Location: " <+> prettyLocation l
        ]
      )
    ExpectedThis l -> PP.align
      (PP.sep
        [ "Found a member reference for a struct/class, but no specifier on LHS!"
        , "Suggestion: Please add a `this->` to all member references/method calls!"
        , "Location: " <+> prettyLocation l
        ]
      )
    DesugarWeirdness l -> "Something weird at: " <+> prettyLocation l

data DesugarEnv = DesugarEnv { _allStructs :: [Struct], _currentCtor :: Maybe ConstructorCursor }
  deriving stock (Eq, Show)
makeLenses ''DesugarEnv

getCurrentStruct :: MonadDesugar m => m (Maybe Struct)
getCurrentStruct = do
  ctor <- view currentCtor
  case ctor of
    Nothing    -> pure Nothing
    Just ctor' -> do
      structs <- view allStructs

      pure $ find (isThisYourCtor ctor') structs
 where
  isThisYourCtor :: ConstructorCursor -> Struct -> Bool
  isThisYourCtor ctor (Struct _ _ _ _ ctors) = ctor `elem` ctors

type MonadDesugar m = (MonadError DesugarError m, MonadReader DesugarEnv m)

desugarExpr :: MonadDesugar m => Cursor -> m Expr
desugarExpr cursor = case cursorKind cursor of
  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    resultTyp <- extractType cursor
    opTyp     <- extractType left

    binOp     <- note (UnknownBinaryOperation (loc cursor))
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)
    let binOpBuiltin = Builtin cursor $ BuiltinBinOp binOp resultTyp opTyp
    pure $ App cursor (App cursor binOpBuiltin leftExpr) rightExpr

  UnaryOperator -> do
    let [child] = cursorChildren cursor

    childExpr <- desugarExpr child

    resultTyp <- extractType cursor
    opTyp     <- extractType child

    unOp      <- note (UnknownUnaryOperation (loc cursor))
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
        child     <- note (WeirdNewOperator $ loc cursor) $ xs ^? _last

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

  Constructor -> do
    referenced <- note (ExpectedReferencedExpr $ loc cursor)
      $ cursorReferenced cursor

    let name = nameFromBS $ cursorSpelling referenced
    pure $ Var cursor name

  CallExpr -> do
    case cursorChildren cursor of
      [child]  -> desugarExpr child
      children -> do
        let (fn : args) = children
        fnExpr <- case cursorKind fn of
          DeclRefExpr -> desugarExpr fn
          TypeRef     -> do
            referenced <- note (ExpectedReferencedExpr $ loc fn)
              $ cursorReferenced cursor
            desugarExpr referenced
          FirstExpr -> desugarExpr fn
          other     -> do
            traceShowM $ "Found: " <> show other
            throwError (DesugarWeirdness $ loc fn)
            -- hope that this is a constructor.
        exprs <- traverse desugarExpr args

        -- TODO: can we write this using Lens?
        let exprs' = case exprs of
              [] -> [unit cursor]
              xs -> xs

        pure $ foldl (App cursor) fnExpr exprs'

  CXXThisExpr -> do
    ctor <- view currentCtor
    case ctor of
      Nothing -> throwError (DesugarWeirdness $ loc cursor)
      Just _  -> do
        (Struct _ t _ _ _) <- fromJust <$> getCurrentStruct
        let this = Builtin cursor (BuiltinThis t)
        pure this

  MemberRefExpr -> case cursorChildren cursor of
    [child] -> do
      expr <- desugarExpr child
      let memberName       = nameFromBS $ cursorSpelling cursor
      let memberSymbolType = TSymbol memberName
      pure
        $ App cursor (Builtin cursor (BuiltinMemberRef memberSymbolType)) expr
    [] -> do
      -- this means that we're in some class currently
      -- and we're using relative access.
      -- 
      -- probably.
      -- it's hard to know for sure, let's just tell the user to annotate their functions with a 'this'!
      throwError (ExpectedThis $ loc cursor)

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

desugarBlock
  :: MonadDesugar m => (Cursor -> Expr) -> T.CursorK 'CompoundStmt -> m Expr
desugarBlock defaultResult cursor = do
  result <- longFunc
  pure $ result $ defaultResult (T.withoutKind cursor)
 where
  go :: MonadDesugar m => (Expr -> Expr) -> Cursor -> m (Expr -> Expr)
  go cont c = do
    resultFn <- desugarBlockOne defaultResult c
    pure $ cont . resultFn

  longFunc :: MonadDesugar m => m (Expr -> Expr)
  longFunc = foldlM go id $ T.cursorChildren cursor

desugarStmt :: MonadDesugar m => (Cursor -> Expr) -> Cursor -> m Expr
desugarStmt defaultResult cursor = case cursorKind cursor of
  CompoundStmt ->
    desugarBlock defaultResult $ fromJust $ T.matchKind @ 'CompoundStmt $ cursor
  _ -> do
    stmtCont <- desugarBlockOne defaultResult cursor
    pure $ stmtCont (defaultResult cursor)

desugarBlockOne
  :: MonadDesugar m => (Cursor -> Expr) -> Cursor -> m (Expr -> Expr)
desugarBlockOne defaultResult cursor = case cursorKind cursor of
  DeclStmt -> do
    let [child] = cursorChildren cursor

    case cursorKind child of
      VarDecl -> do
        grandchild <-
          note (DesugarWeirdness $ loc child) $ cursorChildren child ^? _last

        expr <- desugarExpr grandchild

        let name = nameFromBS $ cursorSpelling child

        pure $ \rest -> LetIn child name expr rest
      _ -> throwError (UnknownDeclarationKindInBlock $ loc child)

  ReturnStmt -> case cursorChildren cursor of
    [child] -> do
      expr <- desugarExpr child

      pure $ \_ -> expr
    [] -> pure $ \_ -> unit cursor
    _  -> throwError (DesugarWeirdness $ loc cursor)

  IfStmt -> case cursorChildren cursor of
    [cond, thn] -> do
      condition <- desugarExpr cond
      expr      <- desugarStmt defaultResult thn

      pure $ \els -> If cursor condition expr els

    [cond, thn, els] -> do
      condition <- desugarExpr cond

      expr1     <- desugarStmt defaultResult thn
      expr2     <- desugarStmt defaultResult els

      pure $ \_ -> If cursor condition expr1 expr2

    _ -> throwError (InvalidIfShape $ loc cursor)

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

    binOp <- note (UnknownBinaryOperation $ loc cursor)
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)

    unless (isAssignOp binOp) $ throwError (NotImplementedYet $ loc cursor)
    unless (not $ isJust $ withoutAssign binOp)
      $ error "+= etc. not supported yet"

    leftExpr  <- desugarExpr left
    rightExpr <- desugarExpr right

    let setter = App cursor
                     (App cursor (Builtin cursor BuiltinAssign) leftExpr)
                     rightExpr

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

desugarTopLevelFunction :: MonadDesugar m => SomeFunctionCursor -> m TopLevel
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
        functionBody   <- desugarBlock unit body

        let function = functionHeader functionBody

        pure $ TLLet $ Let untypedCursor name function
      _ -> throwError (WeirdFunctionBody $ loc cursor)

withConstructor :: MonadDesugar m => ConstructorCursor -> (m a -> m a)
withConstructor c f = locally currentCtor (const $ Just c) $ f

desugarConstructor :: MonadDesugar m => ConstructorCursor -> m TopLevel
desugarConstructor cursor = withConstructor cursor $ do
  -- these are typically the first children but I really want to be safe here 
  let parameterF :: Fold Cursor (T.CursorK 'ParmDecl)
      parameterF = cursorChildrenF . folding (T.matchKind @ 'ParmDecl)

  -- this is typically the last child but again, trying to be safe here!
  let bodyF :: Fold Cursor (T.CursorK 'CompoundStmt)
      bodyF = cursorChildrenF . folding (T.matchKind @ 'CompoundStmt)

  let untypedCursor = T.withoutKind cursor
  let parameters    = untypedCursor ^.. parameterF
  let name          = nameFromBS $ cursorSpelling untypedCursor

  env <- view currentCtor
  traceShowM env

  maybeStruct        <- getCurrentStruct
  (Struct _ t _ _ _) <- note (ExpectedValidStruct $ loc cursor) maybeStruct
  let returnInstance c = Builtin c (BuiltinThis t)

  case untypedCursor ^.. bodyF of
    [body] -> do

      functionHeader <- desugarParameters parameters
      functionBody   <- desugarBlock returnInstance body

      let function = functionHeader functionBody

      pure $ TLLet $ Let untypedCursor name function
    _ -> throwError (WeirdFunctionBody $ loc cursor)

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
    [] -> throwError (DesugarWeirdness $ loc (cursors ^?! _head))
    xs -> xs & NE.fromList & TLLetRecursive & pure

desugarTopLevel
  :: [StructCursor]
  -> [G.SCC SomeFunctionCursor]
  -> Either DesugarError [TopLevel]
desugarTopLevel structs fnSccs = do
  desugaredStructs <- traverse desugarStruct structs
  let desugarEnv = DesugarEnv desugaredStructs Nothing

  tlFns <- traverse (flip runReaderT desugarEnv . desugarSCCTopLevel) fnSccs
  pure $ (TLStruct <$> desugaredStructs) <> tlFns

desugarStruct :: MonadError DesugarError m => StructCursor -> m Struct
desugarStruct cursor = do
  let fields =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'FieldDecl)
  let constructors =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'Constructor)
  let name = nameFromBS $ T.cursorSpelling cursor

  desugaredFields <- traverse desugarField fields

  let tcon = TCon $ TC name StarKind

  let struct =
        Struct (T.withoutKind cursor) tcon name desugaredFields constructors
  pure struct

desugarField
  :: MonadError DesugarError m => T.CursorK 'FieldDecl -> m StructField
desugarField cursor = do
  let untypedCursor = T.withoutKind cursor
  let name          = nameFromBS $ T.cursorSpelling cursor
  typ <- extractType untypedCursor

  pure $ StructField untypedCursor typ name

extractType :: MonadError DesugarError m => Cursor -> m Type
extractType c =
  note (ExpectedValidType $ loc c) $ fromClangType =<< cursorType c

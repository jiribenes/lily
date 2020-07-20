{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Elaboration
  ( elaborateTopLevelFunction
  , elaborateTopLevel
  )
where

import           Control.Lens
import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , runReaderT
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Graph                    as G
import           Data.List                      ( find )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )
import           Debug.Trace                    ( traceM
                                                , traceShowM
                                                )
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

import           Clang
import           Clang.OpParser
import           Core.Located
import           Core.Syntax
import           Error
import           Name                           ( nameIsNull
                                                , Name(Name)
                                                , nameFromBS
                                                )
import           Type.Type                      ( Kind(..)
                                                , TCon(..)
                                                , Type(..)
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Text                     as T

data ElaborationError = WeirdFunctionBody Location
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
                  | ExpectedNamed Location
                  | ElaborationWeirdness Location -- this is a catch-all TODO: replace by appropriate use!
    deriving stock (Show, Eq)

instance Pretty ElaborationError where
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
        [ "TODO: Elaborationing of this AST node is not implemented yet!"
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
    ExpectedNamed l -> PP.align
      (PP.sep
        [ "Found a member reference but Clang doesn't know it's name (RHS)!"
        , "This is really weird!"
        , "Location: " <+> prettyLocation l
        ]
      )
    ElaborationWeirdness l -> "Something weird at: " <+> prettyLocation l

data ElaborationEnv = ElaborationEnv { _allStructs :: [Struct], _currentCtor :: Maybe ConstructorCursor }
  deriving stock (Eq, Show)
makeLenses ''ElaborationEnv

getCurrentStruct :: MonadElaboration m => m (Maybe Struct)
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

type MonadElaboration m = (MonadError ElaborationError m, MonadReader ElaborationEnv m)

elaborateExpr :: MonadElaboration m => Cursor -> m Expr
elaborateExpr cursor = case cursorKind cursor of
  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    leftExpr  <- elaborateExpr left
    rightExpr <- elaborateExpr right

    resultTyp <- extractType cursor
    opTyp     <- extractType left

    binOp     <- note (UnknownBinaryOperation (loc cursor))
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)
    let binOpBuiltin = Builtin cursor $ BuiltinBinOp binOp resultTyp opTyp
    pure $ App cursor (App cursor binOpBuiltin leftExpr) rightExpr

  UnaryOperator -> do
    let [child] = cursorChildren cursor

    childExpr <- elaborateExpr child

    resultTyp <- extractType cursor
    opTyp     <- extractType child

    unOp      <- note (UnknownUnaryOperation (loc cursor))
      $ parseUnOp (fromJust $ T.matchKind @ 'UnaryOperator $ cursor)
    let unOpBuiltin = Builtin cursor $ BuiltinUnOp unOp resultTyp opTyp

    pure $ App cursor unOpBuiltin childExpr

  DeclRefExpr -> do
    let name = nameFromBS $ cursorSpelling cursor

    -- Note: this requires no change in ordering!
    let namespacesF :: Fold Cursor (T.CursorK 'NamespaceRef)
        namespacesF = cursorChildrenF . folding (T.matchKind @ 'NamespaceRef)

    let namespaceName =
          cursor
            ^.. namespacesF
            .   to (decodeUtf8 . T.cursorSpelling)
            &   T.intercalate "::"
            &   Name
    let fullName = if nameIsNull namespaceName
          then name
          else namespaceName <> "::" <> name

    pure $ Var cursor fullName

  CXXNewExpr ->
    -- TODO: This is not correct at all!
    -- look at 'new int' vs 'new int[7]' vs 'new Struct' vs 'new Struct[10]'
                case cursorChildren cursor of
    [] -> do
      typ <- extractType cursor
      pure $ Builtin cursor (BuiltinNew typ)
    xs -> do
      typ       <- extractType cursor
      child     <- note (WeirdNewOperator $ loc cursor) $ xs ^? _last

      childExpr <- elaborateExpr child

      pure $ App cursor (Builtin cursor (BuiltinNewArray typ)) childExpr

  ArraySubscriptExpr -> do
    let [left, right] = cursorChildren cursor

    leftExpr     <- elaborateExpr left
    rightExpr    <- elaborateExpr right

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

  CallExpr -> case cursorChildren cursor of
    [child]  -> elaborateExpr child
    children -> do
      let (fn : args) = children
      fnExpr <- case cursorKind fn of
        DeclRefExpr   -> elaborateExpr fn
        FirstExpr     -> elaborateExpr fn
        MemberRefExpr -> elaborateExpr fn
        TypeRef       -> do
          referenced <- note (ExpectedReferencedExpr $ loc fn)
            $ cursorReferenced cursor
          elaborateExpr referenced
        other -> do
          traceShowM $ "Found: " <> show other
          throwError (ElaborationWeirdness $ loc fn)
          -- hope that this is a constructor.
      exprs <- traverse elaborateExpr args

      -- TODO: can we write this using Lens?
      let exprs' = case exprs of
            [] -> [unit cursor]
            xs -> xs

      pure $ foldl (App cursor) fnExpr exprs'

  CXXThisExpr -> do
    ctor <- view currentCtor
    case ctor of
      Nothing -> throwError (ElaborationWeirdness $ loc cursor)
      Just _  -> do
        (Struct _ t _ _ _) <- fromJust <$> getCurrentStruct
        let this = Builtin cursor (BuiltinThis t)
        pure this

  MemberRefExpr -> case cursorChildren cursor of
    [child] -> do
      expr       <- elaborateExpr child
      memberName <-
        note (ExpectedNamed $ loc cursor)
        $   memberRHSSpelling
        =<< T.matchKind @ 'MemberRefExpr cursor

      let memberSymbolType = TSym memberName
      pure
        $ App cursor (Builtin cursor (BuiltinMemberRef memberSymbolType)) expr
    [] -> do
      -- this means that we're in some class currently
      -- and we're using relative access.
      -- 
      -- probably.
      -- it's hard to know for sure, let's just tell the user to annotate their functions with a 'this'!
      throwError (ExpectedThis $ loc cursor)

  IntegerLiteral        -> elaborateLiteral cursor
  CharacterLiteral      -> elaborateLiteral cursor
  StringLiteral         -> elaborateLiteral cursor
  CXXBoolLiteralExpr    -> elaborateLiteral cursor
  CXXNullPtrLiteralExpr -> pure $ Builtin cursor BuiltinNullPtr

  FirstExpr             -> elaborateSingleChild cursor
  other                 -> error $ "found: " <> show other

elaborateLiteral :: MonadElaboration m => Cursor -> m Expr
elaborateLiteral cursor = do
  typ <- extractType cursor
  pure $ Literal cursor typ

-- | Elaborations a block
elaborateBlock
  :: MonadElaboration m => (Cursor -> Expr) -> T.CursorK 'CompoundStmt -> m Expr
elaborateBlock defaultResult cursor = do
  result <- elaborateBlockCont defaultResult cursor
  pure $ result $ defaultResult (T.withoutKind cursor)

-- | Elaborations a block with a 'Expr'-shaped hole at the end of it
elaborateBlockCont
  :: MonadElaboration m
  => (Cursor -> Expr)
  -> T.CursorK 'CompoundStmt
  -> m (Expr -> Expr)
elaborateBlockCont defaultResult cursor = do
  result <- longFunc
  pure $ result
 where
  go :: MonadElaboration m => (Expr -> Expr) -> Cursor -> m (Expr -> Expr)
  go cont c = do
    resultFn <- elaborateBlockOne defaultResult c
    pure $ cont . resultFn

  longFunc :: MonadElaboration m => m (Expr -> Expr)
  longFunc = foldlM go id $ T.cursorChildren cursor

elaborateStmt :: MonadElaboration m => (Cursor -> Expr) -> Cursor -> m Expr
elaborateStmt defaultResult cursor = case cursorKind cursor of
  CompoundStmt ->
    elaborateBlock defaultResult $ fromJust $ T.matchKind @ 'CompoundStmt $ cursor
  _ -> do
    stmtCont <- elaborateBlockOne defaultResult cursor
    pure $ stmtCont (defaultResult cursor)

elaborateBlockOne
  :: MonadElaboration m => (Cursor -> Expr) -> Cursor -> m (Expr -> Expr)
elaborateBlockOne defaultResult cursor = case cursorKind cursor of
  DeclStmt -> do
    let [child] = cursorChildren cursor

    case cursorKind child of
      VarDecl -> do
        grandchild <-
          note (ElaborationWeirdness $ loc child) $ cursorChildren child ^? _last

        expr <- elaborateExpr grandchild

        let name = nameFromBS $ cursorSpelling child

        pure $ \rest -> LetIn child name expr rest
      _ -> throwError (UnknownDeclarationKindInBlock $ loc child)

  ReturnStmt -> case cursorChildren cursor of
    [child] -> do
      expr <- elaborateExpr child

      pure $ \_ -> expr
    [] -> pure $ \_ -> unit cursor
    _  -> throwError (ElaborationWeirdness $ loc cursor)

  IfStmt -> case cursorChildren cursor of
    [cond, thn] -> do
      condition <- elaborateExpr cond
      expr      <- elaborateStmt defaultResult thn

      pure $ \els -> If cursor condition expr els

    [cond, thn, els] -> do
      condition <- elaborateExpr cond

      expr1     <- elaborateStmt defaultResult thn
      expr2     <- elaborateStmt defaultResult els

      pure $ \_ -> If cursor condition expr1 expr2

    _ -> throwError (InvalidIfShape $ loc cursor)

  CallExpr -> do
      -- TODO: should we force here that this has a void return value?
      --       what to do otherwise with ignored return values?
    expr <- elaborateExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  FirstExpr -> do
      -- TODO: should we force here that this has a void return value?
      --       what to do otherwise with ignored return values?
    expr <- elaborateExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    binOp <- note (UnknownBinaryOperation $ loc cursor)
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)

    unless (isAssignOp binOp) $ throwError (NotImplementedYet $ loc cursor)
    unless (not $ isJust $ withoutAssign binOp)
      $ error "+= etc. not supported yet"

    leftExpr  <- elaborateExpr left
    rightExpr <- elaborateExpr right

    let setter = App cursor
                     (App cursor (Builtin cursor BuiltinAssign) leftExpr)
                     rightExpr

    pure $ \rest -> LetIn cursor (Name "_") setter rest

  CompoundStmt -> do
    -- TODO: we're throwing away some possible semantic information here!
    -- in the following case:
    --
    -- ```
    -- int x = 3;
    -- {
    --   int y = 4;
    --   return y;
    -- }
    -- ```
    block <- elaborateBlock defaultResult
                          (fromJust $ T.matchKind @ 'CompoundStmt cursor)
    pure $ \rest -> LetIn cursor (Name "_") block rest

  other -> do
    -- best effort 
    traceM $ "Encountered: " <> show other <> " in a block. Hopefully it's ok!"
    traceM "I'll interpret it as a normal expression!"
    -- it's not a declaration
    -- so rewrite it as `let _ = <expr> in...`

    expr <- elaborateExpr cursor

    -- TODO: make throwaway name!
    pure $ \rest -> LetIn cursor (Name "_") expr rest

elaborateSingleChild :: MonadElaboration m => Cursor -> m Expr
elaborateSingleChild cursor = do
  let [child] = cursorChildren cursor
  elaborateExpr child

elaborateTopLevelFunction :: MonadElaboration m => SomeFunctionCursor -> m TopLevel
elaborateTopLevelFunction = \case
  SomeConstructor c -> elaborateConstructor c
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

        functionHeader <- elaborateParameters parameters
        functionBody   <- elaborateBlock unit body

        let function = functionHeader functionBody

        pure $ TLLet $ Let untypedCursor LetFunction name function
      _ -> throwError (WeirdFunctionBody $ loc cursor)

-- | Elaboration an expression with the constructor set to the specified value
withConstructor :: MonadElaboration m => ConstructorCursor -> (m a -> m a)
withConstructor c = locally currentCtor (const $ Just c)

elaborateConstructor :: MonadElaboration m => ConstructorCursor -> m TopLevel
elaborateConstructor cursor = withConstructor cursor $ do
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

      functionHeader <- elaborateParameters parameters
      functionBody   <- elaborateBlock returnInstance body

      let function = functionHeader functionBody

      pure $ TLLet $ Let untypedCursor LetConstructor name function
    _ -> throwError (WeirdFunctionBody $ loc cursor)

elaborateParameters
  :: MonadReader ElaborationEnv m
  => MonadElaboration m => [T.CursorK 'ParmDecl] -> m (Expr -> Expr)
elaborateParameters = foldlM go id
 where
  go
    :: MonadElaboration m
    => (Expr -> Expr)
    -> T.CursorK 'ParmDecl
    -> m (Expr -> Expr)
  go cont cursor = do
    let name = nameFromBS $ cursorSpelling $ T.withoutKind cursor
    let lam  = Lam (T.withoutKind cursor) name

    pure $ cont . lam

elaborateSCCTopLevel
  :: MonadReader ElaborationEnv m
  => MonadElaboration m => G.SCC SomeFunctionCursor -> m TopLevel
elaborateSCCTopLevel (G.AcyclicSCC cursor ) = elaborateTopLevelFunction cursor
elaborateSCCTopLevel (G.CyclicSCC  cursors) = do
  topLevelFunctions <- traverse elaborateTopLevelFunction cursors
  let properFunctions = topLevelFunctions ^.. each . folding (preview _TLLet)
  case properFunctions of
    [] -> throwError (ElaborationWeirdness $ loc (cursors ^?! _head))
    xs -> xs & NE.fromList & TLLetRecursive & pure

elaborateTopLevel
  :: [StructCursor]
  -> [G.SCC SomeFunctionCursor]
  -> Either ElaborationError Program
elaborateTopLevel structs fnSccs = do
  elaborateedStructs <- traverse elaborateStruct structs
  let elaborateEnv = ElaborationEnv elaborateedStructs Nothing

  tlFns <- traverse (flip runReaderT elaborateEnv . elaborateSCCTopLevel) fnSccs
  pure $ (TLStruct <$> elaborateedStructs) <> tlFns

elaborateStruct :: MonadError ElaborationError m => StructCursor -> m Struct
elaborateStruct cursor = do
  let fields =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'FieldDecl)
  let constructors =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'Constructor)
  let name = nameFromBS $ T.cursorSpelling cursor

  elaborateedFields <- traverse elaborateField fields

  let tcon = TCon $ TC name StarKind

  let struct =
        Struct (T.withoutKind cursor) tcon name elaborateedFields constructors
  pure struct

elaborateField
  :: MonadError ElaborationError m => T.CursorK 'FieldDecl -> m StructField
elaborateField cursor = do
  let untypedCursor = T.withoutKind cursor
  let name          = nameFromBS $ T.cursorSpelling cursor
  typ <- extractType untypedCursor

  pure $ StructField untypedCursor typ name

extractType :: MonadError ElaborationError m => Cursor -> m Type
extractType c =
  note (ExpectedValidType $ loc c) $ fromClangType =<< cursorType c

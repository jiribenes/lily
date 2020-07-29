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

-- | Represents all possible errors that can happen in the elaboration phase.
data ElaborationError = WeirdFunctionBody Location
                  | UnknownBinaryOperation Location
                  | UnknownUnaryOperation Location
                  | UnknownDeclarationKindInBlock Location
                  | InvalidIfShape Location
                  | ExpectedValidType Location
                  | NotImplementedYet Location
                  | ExpectedValidStruct Location
                  | WeirdNewOperator Location
                  | WeirdDeleteOperator Location
                  | ExpectedReferencedExpr Location
                  | ExpectedThis Location
                  | ExpectedNamed Location
                  | ElaborationWeirdness Location -- this is a catch-all
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
        [ "Elaboration of this AST node is not implemented!"
        , "Location: " <+> prettyLocation l
        ]
      )
    ExpectedValidStruct l -> "Invalid struct at: " <+> prettyLocation l
    WeirdNewOperator l ->
      "Invalid form of the `new` operator at: " <+> prettyLocation l
    WeirdDeleteOperator l ->
      "Invalid form of the `delete` operator at: " <+> prettyLocation l
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

-- | An environment for elaboration contains all 'Struct's and sometimes the 'Constructor' we are working with.
data ElaborationEnv = ElaborationEnv { _allStructs :: [Struct], _currentCtor :: Maybe ConstructorCursor }
  deriving stock (Eq, Show)
makeLenses ''ElaborationEnv

-- | Returns the current struct
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
  isThisYourCtor ctor (Struct _ _ _ _ ctors _) = ctor `elem` ctors

-- | A 'MonadElaboration' computation is a computation that can read the 'ElaborationEnv'
-- and return a 'ElaborationError'
type MonadElaboration m
  = (MonadError ElaborationError m, MonadReader ElaborationEnv m)

-- | Elaborates an expression
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
    -- Warning: This is not correct for all possible shapes of CXXNewExpr. 
    -- e.g. look at 'new int' vs 'new int[7]' vs 'new Struct' vs 'new Struct[10]'
                case cursorChildren cursor of
    [] -> do
      typ <- extractType cursor
      pure $ App cursor
                 (Builtin cursor (BuiltinNew typ))
                 (Builtin cursor BuiltinUnit)
    xs -> do
      typ       <- extractType cursor
      child     <- note (WeirdNewOperator $ loc cursor) $ xs ^? _last

      childExpr <- elaborateExpr child

      pure $ App cursor (Builtin cursor (BuiltinNewArray typ)) childExpr

  CXXDeleteExpr -> case cursorChildren cursor of
    [child] -> do
      expr <- elaborateExpr child
      pure $ App cursor (Builtin cursor BuiltinDelete) expr
    _ -> throwError $ WeirdDeleteOperator $ loc cursor

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
        _ -> do
          throwError (ElaborationWeirdness $ loc fn)
          -- hope that this is a constructor.
      exprs <- traverse elaborateExpr args

      let exprs' = case exprs of
            [] -> [unit cursor]
            xs -> xs

      pure $ foldl (App cursor) fnExpr exprs'

  CXXThisExpr -> do
    ctor <- view currentCtor
    case ctor of
      Nothing -> throwError (ElaborationWeirdness $ loc cursor)
      Just _  -> do
        (Struct _ t _ _ _ _) <- fromJust <$> getCurrentStruct
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

-- | Elaborates a literal
elaborateLiteral :: MonadElaboration m => Cursor -> m Expr
elaborateLiteral cursor = do
  typ <- extractType cursor
  pure $ Literal cursor typ

-- | Elaborates a C++ block, appends the first argument at the end of the block (typically used by implicit return)
elaborateBlock
  :: MonadElaboration m
  => (Cursor -> Expr)
  -> T.CursorK 'CompoundStmt
  -> m Expr
elaborateBlock defaultResult cursor = do
  result <- elaborateBlockCont defaultResult cursor
  pure $ result $ defaultResult (T.withoutKind cursor)

-- | Elaborates a C++ block with a 'Expr'-shaped hole at the end of it,
-- i.e. uses continuation-passing style
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

-- | Elaborates a statement
elaborateStmt :: MonadElaboration m => (Cursor -> Expr) -> Cursor -> m Expr
elaborateStmt defaultResult cursor = case cursorKind cursor of
  CompoundStmt ->
    elaborateBlock defaultResult
      $ fromJust
      $ T.matchKind @ 'CompoundStmt
      $ cursor
  _ -> do
    stmtCont <- elaborateBlockOne defaultResult cursor
    pure $ stmtCont (defaultResult cursor)

-- | Elaborates members of a block in continuation-passing style
elaborateBlockOne
  :: MonadElaboration m => (Cursor -> Expr) -> Cursor -> m (Expr -> Expr)
elaborateBlockOne defaultResult cursor = case cursorKind cursor of
  DeclStmt -> do
    let [child] = cursorChildren cursor

    case cursorKind child of
      VarDecl -> do
        grandchild <-
          note (ElaborationWeirdness $ loc child)
          $  cursorChildren child
          ^? _last

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
    expr <- elaborateExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  FirstExpr -> do
    expr <- elaborateExpr cursor
    pure $ \rest -> LetIn cursor (Name "_") expr rest

  BinaryOperator -> do
    let [left, right] = cursorChildren cursor

    binOp <- note (UnknownBinaryOperation $ loc cursor)
      $ parseBinOp (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)

    unless (isAssignOp binOp) $ throwError (NotImplementedYet $ loc cursor)
    unless (not $ isJust $ withoutAssign binOp)
      $ error "operators like += are not supported"

    leftExpr  <- elaborateExpr left
    rightExpr <- elaborateExpr right

    let setter = App cursor
                     (App cursor (Builtin cursor BuiltinAssign) leftExpr)
                     rightExpr

    pure $ \rest -> LetIn cursor (Name "_") setter rest

  CompoundStmt -> do
    -- Note: we might be throwing away some possible semantic information here!
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
    -- best effort:
    -- it's not a declaration
    -- so rewrite it as `let _ = <expr> in...`

    expr <- elaborateExpr cursor

    pure $ \rest -> LetIn cursor (Name "_") expr rest

-- | A helper function which automatically elaborates an expression
-- with a single child.
elaborateSingleChild :: MonadElaboration m => Cursor -> m Expr
elaborateSingleChild cursor = do
  let [child] = cursorChildren cursor
  elaborateExpr child

-- | Elaborates a top level function as represented by 'SomeFunctionCursor'
elaborateTopLevelFunction
  :: MonadElaboration m => SomeFunctionCursor -> m TopLevel
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

-- | Elaborates an expression with the constructor set to the specified value
withConstructor :: MonadElaboration m => ConstructorCursor -> (m a -> m a)
withConstructor c = locally currentCtor (const $ Just c)

-- | Elaborates a constructor
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

  maybeStruct          <- getCurrentStruct
  (Struct _ t _ _ _ _) <- note (ExpectedValidStruct $ loc cursor) maybeStruct
  let returnInstance c = Builtin c (BuiltinThis t)

  case untypedCursor ^.. bodyF of
    [body] -> do

      functionHeader <- elaborateParameters parameters
      functionBody   <- elaborateBlock returnInstance body

      let function = functionHeader functionBody

      pure $ TLLet $ Let untypedCursor LetConstructor name function
    _ -> throwError (WeirdFunctionBody $ loc cursor)

-- | Elaborates function parameters in continuation-passing style
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

-- | Elaborates the whole strongly connected component of 'SomeFunctionCursor's
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

-- | This is the main function that takes pre-processed C++ AST and returns an elaborated 'Program'
elaborateTopLevel
  :: [StructCursor]
  -> [G.SCC SomeFunctionCursor]
  -> Either ElaborationError Program
elaborateTopLevel structs fnSccs = do
  elaborateedStructs <- traverse elaborateStruct structs
  let elaborateEnv = ElaborationEnv elaborateedStructs Nothing

  tlFns <- traverse (flip runReaderT elaborateEnv . elaborateSCCTopLevel) fnSccs
  pure $ (TLStruct <$> elaborateedStructs) <> tlFns

-- | Elaborates a single struct
elaborateStruct :: MonadError ElaborationError m => StructCursor -> m Struct
elaborateStruct cursor = do
  let fields =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'FieldDecl)
  let constructors =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'Constructor)
  let methodNames =
        cursor ^.. T.cursorChildrenF . folding (T.matchKind @ 'CXXMethod) . to
          (nameFromBS . T.cursorSpelling)
  let name = nameFromBS $ T.cursorSpelling cursor

  elaboratedFields <- traverse elaborateField fields

  let tcon = TCon $ TC name StarKind

  let struct = Struct (T.withoutKind cursor)
                      tcon
                      name
                      elaboratedFields
                      constructors
                      methodNames
  pure struct

-- | Elaborates a field of a struct
elaborateField
  :: MonadError ElaborationError m => T.CursorK 'FieldDecl -> m StructField
elaborateField cursor = do
  let untypedCursor = T.withoutKind cursor
  let name          = nameFromBS $ T.cursorSpelling cursor
  typ <- extractType untypedCursor

  pure $ StructField untypedCursor typ name

-- | Returns the type of a Clang 'Cursor'
extractType :: MonadError ElaborationError m => Cursor -> m Type
extractType c =
  note (ExpectedValidType $ loc c) $ fromClangType =<< cursorType c

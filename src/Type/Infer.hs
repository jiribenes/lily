{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Type.Infer where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Foldable                  ( Foldable(fold)
                                                , traverse_
                                                )
import           Data.List                      ( nub )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )
import           Data.Traversable               ( for )
import           Language.C.Clang.Cursor        ( Cursor
                                                , CursorKind
                                                )

import           Clang.OpParser
import           Control.Monad.Fresh
import           Core.Syntax
import           Name
import qualified Type.Assumption               as A
import qualified Type.Class                    as C
import           Type.Constraint
import           Type.Fresh
import           Type.Simplify
import           Type.Solve
import           Type.Type

-- | Monomorphic variable set
type InferMonos = S.Set TVar

-- | All errors which can be reported during inference
data InferError = FromSolve SolveError
                | UnboundVariables [Name]
                | WrongPredicate Pred
                | WrongConstraint [Constraint]
                | WrongConstructor Name
                | Weirdness -- ^ this just represents an unexpected error which breaks some of my invariants 
                | UnexpectedParameterDeclarationOutsideFunction
                | UnknownASTNodeKind CursorKind
                deriving stock (Eq, Show)

instance Pretty InferError where
  pretty (FromSolve x) = pretty x
  pretty (UnboundVariables as) =
    "Not in scope:" <+> PP.indent 4 (PP.vsep $ pretty <$> as)
  pretty (WrongPredicate   p ) = "Wrong predicate:" <+> pretty p
  pretty (WrongConstructor n ) = "Wrong constructor:" <+> pretty n
  pretty (WrongConstraint  cs) = "Wrong constraints:"
    <+> PP.indent 4 (PP.vsep $ prettyWithReason <$> cs)
    where prettyWithReason c = pretty (c, c ^. reasonL)
  pretty Weirdness = "Encountered general weirdness! What?"
  pretty UnexpectedParameterDeclarationOutsideFunction
    = "Unexpected parameter declaration outside a function declaration. Is your AST ok?"
  pretty (UnknownASTNodeKind k) = PP.align
    (PP.vsep
      [ "Unknown AST node:" <+> PP.squotes (pretty (show k))
      , "Cannot recover automatically, sorry!"
      ]
    )

-- | The state of inference includes a typeEnv ($\Gamma$) of types we have already inferred
-- and a class environemnt 'C.ClassEnv'
data InferState = InferState { _classEnv :: C.ClassEnv, _typeEnv :: M.Map Name Scheme }
makeClassy ''InferState

-- | The initial 'InferState'
initialInferState :: InferState
initialInferState =
  InferState C.initialClassEnv
    $   M.singleton "std::move" -- move is not a linear function, that would make it _single-use_ in the whole function! that is NOT desired!
    $   Forall [varA, varF]
    $   [PFun f]
    :=> (f `TAp` a `TAp` a)
 where
  varA = TV "a" StarKind
  varF = TV "f" arrowKind
  a    = TVar varA
  f    = TVar varF

-- | Infer monad allows to: read monomorphic variables, create fresh variables and raise errors
newtype Infer a = Infer { unInfer :: ReaderT InferMonos (StateT InferState (FreshT Name (Except InferError))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader InferMonos, MonadFresh Name, MonadState InferState, MonadError InferError)

-- | Helper function to actually run the 'Infer' monad with sensible defaults.
runInfer :: Infer a -> Either InferError a
runInfer m =
  m
    & unInfer
    & flip runReaderT S.empty
    & flip evalStateT initialInferState
    & flip evalFreshT initialFreshState
    & runExcept

-- | Take the accumulated constraints and run a solver over them
-- 
-- Doesn't actually _apply_ the substitution!
-- That's up to the caller.
runSolve :: [Constraint] -> Infer (Subst, [Constraint])
runSolve cs = do
  freshSt <- getFresh
  env     <- use classEnv

  case runSolveT env freshSt (solve cs) of
    Left  err                -> throwError $ FromSolve err
    Right (result, newFresh) -> do
      setFresh newFresh
      pure result

-- | Infer type schemes for a 'TopLevel' declaration and store it in 'InferState'
inferTopLevel :: TopLevel -> Infer ()
inferTopLevel tl = do
  m <-
    traverse
        (\(subst, preds, ty) ->
          closeOver (subst `apply` preds) (subst `apply` ty)
        )
      =<< inferType tl
  typeEnv <>= m

-- | This is the top-level function that should be used for inferring a type of something
inferProgram :: Program -> Either InferError InferState
inferProgram tls = runInfer $ traverse_ inferTopLevel tls *> get

-- | Infer a 'Subst'itution, list of 'Pred'icates and a 'Type' for a 'TopLevel' declaration
inferType :: TopLevel -> Infer (M.Map Name (Subst, [Pred], Type))
inferType = \case
  TLStruct s -> do
    classEnv . C.instances %= C.updateInstances (C.hasFieldForStruct s)
    pure mempty
  TLLet (Let _ _ name expr) -> do
    (as, t, cs) <- infer expr

    boundNames  <- use (typeEnv . to M.keysSet)
    let unbounds = A.keysSet as `S.difference` boundNames

    -- if we still have some free unknown variables which are 
    -- in `as` but not in `env` by this point, we need to report them as an error
    unless (S.null unbounds) $ throwError $ UnboundVariables (S.toList unbounds)

    -- pair known assumptions to environment types
    env <- use (typeEnv . to M.toList)
    let cs' =
          [ CExpInst (PairedAssumption x u s) u s
          | (x, s) <- env
          , u      <- x `A.lookup` as
          ]

    -- solve all constraints together and get the resulting substitution
    (subst, unsolved) <- runSolve $ cs <> cs'

    let isPredicate CIn{} = True
        isPredicate _     = False

    let unsolvedNonPredicates = filter (not . isPredicate) unsolved
    unless (null unsolvedNonPredicates) $ throwError $ WrongConstraint
      unsolvedNonPredicates

    pure $ M.singleton
      name
      (subst, subst `apply` toPreds unsolved, subst `apply` t)

  TLLetRecursive lets -> do
    let (names, exprs) = unzip $ NE.toList $ lets <&> \(Let _ _ n e) -> (n, e)

    tyVars <- traverse (const $ freshType StarKind) $ zip names exprs
    let recursiveAssumptions = A.empty `A.extendMany` zip names tyVars

    (ass, ts, css) <- unzip3 <$> traverse infer exprs
    let as = fold ass <> recursiveAssumptions

    boundNames <- use (typeEnv . to M.keysSet)
    let unbounds                = A.keysSet as `S.difference` boundNames
    let unboundsExceptRecursive = unbounds `S.difference` S.fromList names

    -- if we still have some free unknown variables which are 
    -- in `as` but not in `env` by this point, we need to report them as an error
    unless (S.null unboundsExceptRecursive) $ throwError $ UnboundVariables
      (S.toList unbounds)

    -- pair known assumptions to environment types
    env <- use (typeEnv . to M.toList)
    let cs' =
          [ CExpInst (PairedAssumption x t s) t s
          | (x, s) <- env
          , t      <- x `A.lookup` as
          ]

    -- solve all constraints together and get the resulting substitution
    solverResults <- traverse (runSolve . (<> cs')) css

    results       <- for (zip ts solverResults) $ \(t, (subst, unsolved)) -> do
      let isPredicate CIn{} = True
          isPredicate _     = False

      let unsolvedNonPredicates = filter (not . isPredicate) unsolved
      unless (null unsolvedNonPredicates) $ throwError $ WrongConstraint
        unsolvedNonPredicates

      let preds = subst `apply` toPreds unsolved
      let t'    = subst `apply` t

      pure $ (subst, preds, t')

    let nameAndResult = zip names results
    pure $ M.fromList nameAndResult

-- | Closes over a type and generalizes it
closeOver :: [Pred] -> Type -> Infer Scheme
closeOver preds t = do
  env <- use classEnv
  let sch = generalize (fromPred BecauseCloseOver <$> preds) S.empty t
  pure $ normalize env sch

-- | Executes a 'Infer' computation with additional monotype
extendMono :: TVar -> Infer a -> Infer a
extendMono x = local (S.insert x)

-- | Executes a 'Infer' computation with additional monotypes
extendMonos :: [TVar] -> Infer a -> Infer a
extendMonos xs = local (<> S.fromList xs)

makeArrow :: Type -> Type -> Type -> Type
makeArrow x f y = f `TAp` x `TAp` y

-- | Adds a @Geq u t@ constraint for every type @u@ in the assumptions
leq :: Type -> A.Assumption Type -> [Constraint]
leq t = fmap (\x -> CGeq BecauseLeq x t) . A.values

-- | Adds a 'CUn' constraint to type @t@ if the name @x@ is in assumptions @as@.
--
-- This is used for cases when we're introducing @x@ in a new context.
-- If it doesn't get used (i.e. it is not in @as@), it needs to be unrestricted!
wkn :: Name -> Type -> A.Assumption Type -> [Constraint]
wkn x t as | x `A.notMember` as = [CUn BecauseWkn t]
           | otherwise          = []

-- | Adds a 'Un' constraint for every type in the assumptions
unrestricted :: A.Assumption Type -> [Constraint]
unrestricted = fmap (CUn BecauseUn) . A.values

-- | Creates a fresh function with some juicy constraints!
--
-- Note: this requires expr only in order to have better diagnostics!
freshFun :: Expr -> Infer (Type, [Constraint])
freshFun expr = do
  f <- freshType arrowKind
  pure (f, because (BecauseExpr expr) <$> [CFun BecauseFun f])

-- | Takes an expression and produce assumptions, result type and constraints to be solved
infer :: Expr -> Infer (A.Assumption Type, Type, [Constraint])
infer expr = case expr of
  Literal _ typ -> do
    pure (A.empty, typ, [])

  -- A variable x has type t as an assumption  
  Var _ x -> do
    tv <- freshType StarKind
    pure (A.singleton (x, tv), tv, [])

  Lam _ x e -> do
    (tv, a)     <- freshTypeAndTVar StarKind
    (as, t, cs) <- a `extendMono` infer e

    (f, fPreds) <- freshFun expr -- this is the arrow kind
    let as'   = as `A.remove` x

    let preds = because (BecauseExpr expr) <$> nub (leq f as' <> wkn x tv as)

    pure
      ( as'
      , makeArrow tv f t
      , cs
      <> [ CEq (BecauseFunction expr x e) t' tv | t' <- A.lookup x as ]
      <> fPreds
      <> preds
      )

  App _ e1 e2 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    tv             <- freshType StarKind

    (f, fPreds)    <- freshFun expr -- fresh arrow kind
    let as    = as1 `A.intersection` as2
    let preds = nub $ unrestricted $ as

    pure
      ( as1 <> as2
      , tv
      , cs1
      <> cs2
      <> [CEq (BecauseApp expr e1 e2) t1 (makeArrow t2 f tv)]
      <> fPreds
      <> preds
      )

  LetIn _ x e1 e2 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    monos          <- ask

    let as = as1 `A.intersection` as2
    let preds =
          because (BecauseExpr expr) <$> (nub $ unrestricted as <> wkn x t1 as2) -- Q in paper

    pure
      ( as1 <> as2 `A.remove` x
      , t2
      , cs1
      <> cs2
      <> [ CImpInst (BecauseExpr expr) t' (TVar `S.map` monos) t1
         | t' <- A.lookup x as2
         ]
      <> preds
      )

  If _ e1 e2 e3 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    (as3, t3, cs3) <- infer e3

    let as =
          (as1 `A.intersection` (as2 <> as3))
            <> (as2 `A.removeMany` (A.keys as3))
            <> (as3 `A.removeMany` (A.keys as2))
    let preds = because (BecauseExpr expr) <$> (nub $ unrestricted as)

    pure
      ( as1 <> as2 <> as3
      , t2
      , cs1
      <> cs2
      <> cs3
      <> preds
      <> [ CEq (BecauseIfCondition expr e1)   t1 typeBool
         , CEq (BecauseIfBranches expr e2 e3) t2 t3
         ]
      )

  Builtin cursor b -> inferBuiltin expr cursor b

-- | Infer a type for a 'BuiltinExpr'
inferBuiltin
  :: Expr
  -> Cursor
  -> BuiltinExpr
  -> Infer (A.Assumption Type, Type, [Constraint])
inferBuiltin expr _ = \case
  BuiltinBinOp bo resultTyp opTyp -> case bo of
    BinOpEQ -> do
      tv1 <- freshType StarKind
      tv2 <- freshType StarKind

      let f = UnArrow tv1 (UnArrow tv2 typeBool)

      pure (A.empty, f, [CEq (BecauseExpr expr) tv1 tv2])

    _ -> do
      tv1      <- freshType StarKind
      tv2      <- freshType StarKind
      resultTv <- freshType StarKind

      let f = UnArrow tv1 (UnArrow tv2 resultTv)

      pure
        ( A.empty
        , f
        , [ CEq (FromClang resultTyp expr) resultTv resultTyp
          , CEq (BecauseExpr expr)         tv1      tv2
          , CEq (FromClang opTyp expr)     tv1      opTyp
          ]
        )
  BuiltinUnOp uo resultTyp opTyp -> case uo of
    UnOpAddrOf -> do
      tv  <- freshType StarKind
      tv' <- freshType StarKind
      let resultType = typePtrOf tv'
      let f          = UnArrow tv resultType

      pure (A.empty, f, [CEq (BecauseExpr expr) tv tv'])
    UnOpDeref -> do
      tv         <- freshType StarKind
      resultType <- freshType StarKind
      let f = UnArrow (typePtrOf tv) resultType

      pure (A.empty, f, [CEq (BecauseExpr expr) tv resultType])
    _ -> do
      tv1      <- freshType StarKind
      resultTv <- freshType StarKind

      let f = UnArrow tv1 resultTv

      pure
        ( A.empty
        , f
        , [ CEq (FromClang resultTyp expr) resultTv resultTyp
          , CEq (FromClang opTyp expr)     tv1      opTyp
          ]
        )
  BuiltinMemberRef fieldNameType -> do
    recordType <- freshType StarKind
    memberType <- freshType StarKind

    let f = UnArrow recordType memberType
    pure
      ( A.empty
      , f
      , [CHasField (BecauseExpr expr) fieldNameType recordType memberType]
      )
  BuiltinArraySubscript clangArrayTyp clangSubscriptTyp -> do
    array     <- freshType (ArrowKind StarKind StarKind)
    arrayElem <- freshType StarKind
    subscript <- freshType StarKind
    result    <- freshType StarKind

    let arrType = array `TAp` arrayElem

    let f       = UnArrow arrType (UnArrow subscript result)

    pure
      ( A.empty
      , f
      , [ CEq (FromClang clangArrayTyp expr)     clangArrayTyp     arrType
        , CEq (BecauseExpr expr)                 result            arrayElem
        , CEq (FromClang clangSubscriptTyp expr) clangSubscriptTyp subscript
        ]
      )
  BuiltinUnit    -> pure (A.empty, typeUnit, [])
  BuiltinNullPtr -> do
    tv    <- freshType StarKind

    someA <- freshType StarKind
    let somePointer = typePtrOf someA

    pure (A.empty, tv, [CEq (BecauseExpr expr) tv somePointer])
  BuiltinNewArray typ -> do
    tv         <- freshType StarKind
    resultType <- freshType StarKind

    let f = UnArrow tv resultType

    pure (A.empty, f, [CEq (FromClang typ expr) typ resultType])
  BuiltinNew typ -> do
    tv       <- freshType StarKind
    resultTv <- freshType StarKind

    let somePointer = typePtrOf resultTv
    let f           = UnArrow tv somePointer

    pure
      ( A.empty
      , f
      , [ CEq (FromClang typ expr) somePointer typ
        , CEq (BecauseExpr expr)   tv          typeUnit
        ]
      )
  BuiltinDelete -> do
    tv         <- freshType StarKind
    resultType <- freshType StarKind

    let f = UnArrow (typePtrOf tv) resultType
    pure (A.empty, f, [CEq (BecauseExpr expr) resultType typeUnit])
  BuiltinAssign -> do
    tv          <- freshType StarKind

    (f, fPreds) <- freshFun expr

    let result = makeArrow tv typeLinArrow $ makeArrow tv f typeUnit

    pure (A.empty, result, fPreds <> [CGeq (BecauseExpr expr) tv f])
  BuiltinThis typ -> pure (A.empty, typ, [])

-- | Attempt to convert the type scheme into a normalized format  
normalize :: C.ClassEnv -> Scheme -> Scheme
normalize env (Forall origVars (origPreds :=> origBody)) = Forall
  (M.elems sub)
  (   (properSub `apply` normpreds (S.fromList $ filter isPredRelevant preds))
  :=> normtype body
  )
 where
  Forall _ (preds :=> body) = simplifyScheme
    $ Forall origVars (normpreds (S.fromList origPreds) :=> origBody)
  usefulVars = S.toList $ S.fromList origVars `S.intersection` ftv body
  sub        = M.fromList $ (\(old@(TV _ k), n) -> (old, TV n k)) <$> zip
    usefulVars
    (Name . ("t" <>) . Text.pack . show @Int <$> [1 ..])

  properSub = Subst $ M.map TVar sub

  normtype :: Type -> Type
  normtype (TAp a b)  = normtype a `TAp` normtype b
  normtype con@TCon{} = con
  normtype sym@TSym{} = sym
  normtype (TVar a)   = case a `M.lookup` sub of
    Just x  -> TVar x
    Nothing -> error "tv not in signature"

  normpreds :: S.Set Pred -> [Pred]
  normpreds ps = S.toList $ S.filter (\p -> not $ p `C.member` env) ps

  isPredRelevant :: Pred -> Bool
  isPredRelevant (IsIn _ ts) = null (filter (\t -> t `M.notMember` sub) tyVars) -- check if we haven't lost _any_!
    where tyVars = filterTVars . NE.toList $ ts

  toTVar :: Type -> Maybe TVar
  toTVar (TVar t) = Just t
  toTVar _        = Nothing

  filterTVars :: [Type] -> [TVar]
  filterTVars = catMaybes . fmap toTVar

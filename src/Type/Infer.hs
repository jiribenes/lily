{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Type.Infer where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable                  ( Foldable(fold) )
import           Data.List                      ( nub )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as Text
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )
import           Data.Traversable               ( for )
import           Language.C.Clang.Cursor        ( CursorKind )
import           Data.Maybe                     ( catMaybes )

import           Clang.OpParser
import           Control.Monad.Fresh
import           Core.Syntax
import           Error
import           Name
import qualified Type.Assumption               as A
import           Type.Constraint
import           Type.Simplify
import qualified Type.Solve as S
import           Type.Type
import qualified Type.Class as C

import Control.Monad.State.Strict
import Debug.Trace (trace)
import Debug.Trace.Pretty

newtype InferEnv = InferEnv { _typeEnv :: M.Map Name Scheme }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''InferEnv

-- | Monomorphic variable set
type InferMonos = S.Set TVar

data InferError = FromSolve S.SolveError
                | UnboundVariables [Name]
                | WrongPredicate Pred
                | WrongConstraint [Constraint]
                | WrongConstructor Name
                | Weirdness -- TODO: elaborate on this further!
                | UnexpectedParameterDeclarationOutsideFunction
                | UnknownASTNodeKind CursorKind

instance Pretty InferError where
  pretty (FromSolve x) = pretty x
  pretty (UnboundVariables as) =
    "Not in scope:" <+> PP.indent 4 (PP.vsep $ pretty <$> as)
  pretty (WrongPredicate   p) = "Wrong predicate:" <+> pretty p
  pretty (WrongConstructor n) = "Wrong constructor:" <+> pretty n
  pretty (WrongConstraint cs) =
    "Wrong constraints:" <+> PP.indent 4 (PP.vsep $ pretty <$> cs)
  pretty Weirdness = "Encountered general weirdness! What?"
  pretty UnexpectedParameterDeclarationOutsideFunction
    = "Unexpected parameter declaration outside a function declaration. Is your AST ok?"
  pretty (UnknownASTNodeKind k) = PP.align
    (PP.vsep
      [ "Unknown AST node:" <+> PP.squotes (pretty (show k))
      , "Cannot recover automatically, sorry!"
      ]
    )
    -- show (Ambiguous cs) = unlines ["Cannot match expected type: " <> show a <> " with actual type: " <> show b  | (a, b) <- cs]
    -- show (WrongKind a b) = "Wrong kind!"

newtype InferState = InferState { _classEnv :: C.ClassEnv }
makeClassy ''InferState

-- | Infer monad allows to: read monomorphic variables, create fresh variables and raise errors
newtype Infer a = Infer { unInfer :: ReaderT InferMonos (StateT InferState (FreshT Name (Except InferError))) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader InferMonos, MonadFresh Name, MonadState InferState, MonadError InferError)

-- | Helper function to actually run the 'Infer' monad with sensible defaults.
runInfer :: Infer a -> Either InferError a
runInfer m = m
           & unInfer
           & flip runReaderT S.empty
           & flip evalStateT (InferState C.initialClassEnv)
           & flip evalFreshT initialFreshState
           & runExcept

-- | Take the accumulated constraints and run a solver over them
-- 
-- Doesn't actually _apply_ the substitution!
-- That's up to the caller.
runSolve :: [Constraint] -> Infer (Subst, [Constraint])
runSolve cs = do
  freshSt <- getFresh
  env <- use classEnv

  case S.runSolveT env freshSt (S.solve cs) of
    Left  err                -> throwError $ FromSolve err
    Right (result, newFresh) -> do
      setFresh newFresh
      pure result

-- | Infer type schemes for a 'TopLevel' declaration.
inferTopLevel :: InferEnv -> TopLevel -> Either InferError (M.Map Name Scheme)
inferTopLevel env tl = runInfer (inferAndCloseOver env tl) 

inferAndCloseOver :: InferEnv -> TopLevel -> Infer (M.Map Name Scheme)
inferAndCloseOver env tl = do
    m <- inferType env tl
    traverse
      (\(subst, preds, ty) -> closeOver (subst `apply` preds) (subst `apply` ty))
      m


-- | Infer a 'Subst'itution, list of 'Pred'icates and a 'Type' for a 'TopLevel' declaration
inferType :: InferEnv -> TopLevel -> Infer (M.Map Name (Subst, [Pred], Type))
inferType env = \case
  TLStruct _                 -> pure $ mempty
  TLLet    (Let _ name expr) -> do
    (as, t, cs) <- infer expr

    let unbounds = A.keysSet as `S.difference` (env ^. typeEnv . to M.keysSet)

    -- if we still have some free unknown variables which are 
    -- in `as` but not in `env` by this point, we need to report them as an error
    unless (S.null unbounds) $ throwError $ UnboundVariables (S.toList unbounds)

    -- pair known assumptions to environment types
    let cs' =
          [ CExpInst (PairedAssumption x t) t s
          | (x, s) <- env ^. typeEnv . to M.toList
          , t      <- x `A.lookup` as
          ]

    -- solve all constraints together and get the resulting substitution
    let prettify x =
            show (x & pretty, x ^. reasonL & pretty, t & pretty)
    let go cs =
            runSolve $ trace
              (unlines $ prettify <$> (cs <> cs'))
                       cs <> cs'
    (subst, unsolved) <- go $ cs <> cs'

    -- Ideally, do a difference between these lists, but that's too hard for now
    -- TODO(jb): ^ I don't understand what I meant by this. Damn.
    unless (length unsolved == length (S.toPreds unsolved))
      $ throwError
      $ WrongConstraint unsolved

    pure $ M.singleton
      name
      (subst, subst `apply` S.toPreds unsolved, subst `apply` t)

  TLLetRecursive lets -> do
    let (names, exprs) = unzip $ NE.toList $ lets <&> \(Let _ n e) -> (n, e)

    tyVars <- traverse (const $ S.freshType StarKind) $ zip names exprs
    let recursiveAssumptions = A.empty `A.extendMany` zip names tyVars

    (ass, ts, css) <- unzip3 <$> traverse infer exprs
    let as = fold ass <> recursiveAssumptions

    let unbounds = A.keysSet as `S.difference` (env ^. typeEnv . to M.keysSet)
    let unboundsExceptRecursive = unbounds `S.difference` S.fromList names

    -- if we still have some free unknown variables which are 
    -- in `as` but not in `env` by this point, we need to report them as an error
    unless (S.null unboundsExceptRecursive) $ throwError $ UnboundVariables
      (S.toList unbounds)

    -- pair known assumptions to environment types
    let cs' =
          [ CExpInst (PairedAssumption x t) t s
          | (x, s) <- env ^. typeEnv . to M.toList
          , t      <- x `A.lookup` as
          ]

    -- solve all constraints together and get the resulting substitution
    solverResults <- do
      let relevantConstraints cs = cs <> cs'
      let prettify x =
            show (x & pretty, x ^. reasonL & pretty, ts ^?! _head & pretty)
      let go cs =
            runSolve $ -- trace
            --(unlines $ prettify <$> relevantConstraints cs)
                       relevantConstraints cs
      traverse go css

    results <- for (zip ts solverResults) $ \(t, (subst, unsolved)) -> do
      let preds = subst `apply` S.toPreds unsolved
      let t'    = subst `apply` t

      --Ideally, do a difference between these lists, but that's too hard for now
      unless (length unsolved == length (S.toPreds unsolved))
        $ throwError
        $ WrongConstraint unsolved

      pure $ {-traceShow (pretty subst, pretty preds, pretty t')-}
             (subst, preds, t')

    let nameAndResult = zip names results
    pure $ M.fromList nameAndResult

closeOver :: [Pred] -> Type -> Infer Scheme
closeOver preds t = do
  env <- use classEnv
  let sch = S.generalize (S.fromPred BecauseCloseOver <$> preds) S.empty t
  pure $ normalize env sch

-- | Executes a 'Infer' computation with additional monotype
extendMono :: TVar -> Infer a -> Infer a
extendMono x = local (S.insert x)

-- | Executes a 'Infer' computation with additional monotypes
extendMonos :: [TVar] -> Infer a -> Infer a
extendMonos xs = local (<> S.fromList xs)

-- TODO: Audit 'makeFn2' and 'makeFn3'
-- as they are using a type that's not really helpful!
--
-- We don't want to use 'typeArrow' anywhere,
-- and ideally we'd like to deprecate it completely!
makeFn2 :: Type -> Type -> Type
makeFn2 a b = typeArrow `TAp` a `TAp` b

makeFn3 :: Type -> Type -> Type -> Type
makeFn3 a b c = typeArrow `TAp` a `TAp` makeFn2 b c

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
  f <- S.freshType arrowKind
  pure (f, because (BecauseExpr expr) <$> [CFun BecauseFun f])

-- | Take an expression and produce assumptions, result type and constraints to be solved
infer :: Expr -> Infer (A.Assumption Type, Type, [Constraint])
infer expr = case expr of
  Literal cursor typ -> do
    pure (A.empty, typ, [])

  -- A variable x has type t as an assumption  
  Var cursor x -> do
    tv <- S.freshType StarKind
    pure (A.singleton (x, tv), tv, [])

  -- An abstraction \x -> e:
  --  - We'll presume that x has a monomorphic type tv
  --  - Provided e produces assumptions as, type t and constraints cs
  --  - We produce:
  --        - Assumptions as - x
  --        - Type tv -f> t, where f is a new type variable of arrow kind
  --        - Constraints:
  --           - t' ~ tv where t' is type of x in as
  --           - cs
  --           - Fun f
  --           - leq f (as - x)
  --           - wkn x tv as
  Lam cursor x e -> do
    (tv, a)     <- S.freshTypeAndTVar StarKind
    (as, t, cs) <- a `extendMono` infer e

    (f, fPreds) <- freshFun expr -- this is the arrow kind
    let as'   = as `A.remove` x

    let preds = because (BecauseExpr expr) <$> nub (leq f as' <> wkn x tv as)

    pure
      ( as'
      , makeArrow tv f t
      , cs
      <> [ CEq (BecauseExpr e) t' tv | t' <- A.lookup x as ]
      <> fPreds
      <> preds
      )

  App cursor e1 e2 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    tv             <- S.freshType StarKind

    (f, fPreds)    <- freshFun expr -- fresh arrow kind
    let preds = nub $ unrestricted $ as1 `A.intersection` as2

    pure
      ( as1 <> as2
      , tv
      , cs1
      <> cs2
      <> [CEq (BecauseExpr expr) t1 (makeArrow t2 f tv)]
      <> fPreds
      <> preds
      )

  LetIn cursor x e1 e2 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    monos          <- ask
    tv             <- S.freshType StarKind
    let preds = because (BecauseExpr expr) <$> (nub $ unrestricted (as1 `A.intersection` as2) <> wkn x t1 as2) -- Q in paper

    pure
      ( as1 <> as2 `A.remove` x
      , t2
      , cs1
      <> cs2
      <> [ CImpInst (BecauseExpr expr) t' monos t1 | t' <- A.lookup x as2 ]
      <> [ CEq (BecauseExpr expr) tv t' | t' <- A.lookup x as2 ] -- This is for the wkn condition, as we need a new type variable
      <> preds
      )

  If cursor e1 e2 e3 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    (as3, t3, cs3) <- infer e3
    pure
      ( as1 <> as2 <> as3
      , t2
      , cs1
      <> cs2
      <> cs3
      <> [CEq (BecauseExpr expr) t1 typeBool, CEq (BecauseExpr expr) t2 t3]
      )

  Builtin cursor b -> case b of
    BuiltinBinOp bo resultTyp opTyp -> case bo of
      -- TODO: scrap the boilerplate, use resultTyp and opTyp directly!
      BinOpEQ -> do
        tv1      <- S.freshType StarKind
        tv2      <- S.freshType StarKind
        resultTv <- S.freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv typeBool
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpAdd -> do -- TODO: B O I L E R P L A T E
        tv1      <- S.freshType StarKind
        tv2      <- S.freshType StarKind
        resultTv <- S.freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv tv1
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpSub -> do -- TODO: B O I L E R P L A T E
        tv1      <- S.freshType StarKind
        tv2      <- S.freshType StarKind
        resultTv <- S.freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv tv1
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpMul -> do
        tv1      <- S.freshType StarKind
        tv2      <- S.freshType StarKind
        resultTv <- S.freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv tv1
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      _ -> error "not implemented yet"
    BuiltinUnOp uo resultTyp opTyp -> case uo of
      -- TODO: scrap the boilerplate _for most!_, use resultTyp and opTyp directly!
      UnOpAddrOf -> do
        tv  <- S.freshType StarKind
        tv' <- S.freshType StarKind
        let resultType = typePtrOf tv'
        let f          = makeFn2 tv resultType

        pure (A.empty, f, [CEq (BecauseExpr expr) tv tv'])
      UnOpDeref -> do
        tv         <- S.freshType StarKind
        resultType <- S.freshType StarKind
        let f = makeFn2 (typePtrOf tv) resultType

        pure (A.empty, f, [CEq (BecauseExpr expr) tv resultType])
      other -> error $ "not implemented yet: " <> show other
    BuiltinMemberRef -> error "not implemented yet"
    BuiltinArraySubscript clangArrayTyp clangSubscriptTyp -> do
      array     <- S.freshType (ArrowKind StarKind StarKind)
      arrayElem <- S.freshType StarKind
      subscript <- S.freshType StarKind
      result    <- S.freshType StarKind

      let arrType = array `TAp` arrayElem

      let f       = makeFn3 arrType subscript result

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
      tv    <- S.freshType StarKind

      someA <- S.freshType StarKind
      let somePointer = typePtrOf someA

      pure (A.empty, tv, [CEq (BecauseExpr expr) tv somePointer])
    BuiltinNewArray typ -> do
      tv         <- S.freshType StarKind
      resultType <- S.freshType StarKind

      let f = makeFn2 tv resultType

      pure (A.empty, f, [CEq (FromClang typ expr) typ resultType])
    BuiltinNew typ -> do
      resultType <- S.freshType StarKind

      pure (A.empty, resultType, [CEq (FromClang typ expr) typ resultType])
    BuiltinSet -> do
      tv <- S.freshType StarKind

      let settee = MutRef tv

      (f, fPreds)    <- freshFun expr

      let result = makeArrow settee f $ makeArrow tv typeLinArrow typeUnit

      pure (A.empty, result, fPreds) 

-- | This is the top-level function that should be used for inferring a type of something
inferTop :: InferEnv -> [TopLevel] -> Either InferError InferEnv
inferTop env []         = Right env
inferTop env (tl : tls) = case inferTopLevel env tl of
  Left  err -> Left err
  Right m   -> inferTop (env & typeEnv <>~ m) tls

-- | Attempt to convert the type scheme into a normal(ish) format  
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
  normtype (TVar a)   = case a `M.lookup` sub of
    Just x  -> TVar x
    Nothing -> error "tv not in signature"

  normpreds :: S.Set Pred -> [Pred]
  normpreds ps = S.toList $ S.filter (\p -> not $ p `C.inEnv` env) ps

  isPredRelevant :: Pred -> Bool
  isPredRelevant (IsIn _ ts) = null (filter (\t -> t `M.notMember` sub) tyVars) -- check if we haven't lost _any_!
    where tyVars = filterTVars . NE.toList $ ts

  toTVar :: Type -> Maybe TVar
  toTVar (TVar t) = Just t
  toTVar _        = Nothing

  filterTVars :: [Type] -> [TVar]
  filterTVars = catMaybes . fmap toTVar

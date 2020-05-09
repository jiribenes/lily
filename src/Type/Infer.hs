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

import           Clang.OpParser
import           Control.Monad.Fresh
import           Core.Syntax
import           Error
import           Name
import qualified Type.Assumption               as A
import           Type.Constraint
import           Type.Simplify
import           Type.Solve
import           Type.Type


newtype InferEnv = InferEnv { _typeEnv :: M.Map Name Scheme }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''InferEnv

-- | Monomorphic variable set
type InferMonos = S.Set TVar

data InferError = FromSolve SolveError
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
  pretty (WrongPredicate   pred) = "Wrong predicate:" <+> pretty pred
  pretty (WrongConstructor n   ) = "Wrong constructor:" <+> pretty n
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

-- | Infer monad allows to: read monomorphic variables, create fresh variables and raise errors
newtype Infer a = Infer { unInfer :: ReaderT InferMonos (FreshT Name (Except InferError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader InferMonos, MonadFresh Name, MonadError InferError)

runInfer :: Infer a -> Either InferError a
runInfer m =
  runExcept $ evalFreshT (runReaderT (unInfer m) S.empty) initialFreshState

-- | Infer a single type for a top-level
inferTopLevel :: InferEnv -> TopLevel -> Either InferError (M.Map Name Scheme)
inferTopLevel env tl = case runInfer (inferType env tl) of
  Left  err -> Left err
  Right m   -> Right $ M.map
    (\(subst, preds, ty) -> closeOver (subst `apply` preds) (subst `apply` ty))
    m

-- | Take the accumulated constraints and run a solver over them
runSolve :: [Constraint] -> Infer (Subst, [Constraint])
runSolve cs = do
  freshSt <- getFresh
  case runSolveT freshSt (solve cs) of
    Left  err                -> throwError $ FromSolve err
    Right (result, newFresh) -> do
      setFresh newFresh
      pure result

inferType :: InferEnv -> TopLevel -> Infer (M.Map Name (Subst, [Pred], Type))
inferType env = \case
  TLLet (Let _ name expr) -> do
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
    (subst, unsolved) <- runSolve (cs <> cs')

    -- Ideally, do a difference between these lists, but that's too hard for now
    -- TODO(jb): ^ I don't understand what I meant by this. Damn.
    unless (length unsolved == length (toPreds unsolved))
      $ throwError
      $ WrongConstraint unsolved

    pure $ M.singleton
      name
      (subst, subst `apply` toPreds unsolved, subst `apply` t)

  TLLetRecursive lets -> do
    let nameAndExpr = lets <&> \(Let _ n e) -> (n, e)

    tyVars <- traverse (const $ freshType StarKind) nameAndExpr
    let nameAndTyVar = NE.zip nameAndExpr tyVars <&> \((n, e), tv) -> (n, tv)
    let recursiveAssumptions = A.empty `A.extendMany` NE.toList nameAndTyVar

    (ass, ts, css) <- unzip3 <$> traverse infer (nameAndExpr ^.. each . _2)
    let as       = fold ass <> recursiveAssumptions

    let unbounds = A.keysSet as `S.difference` (env ^. typeEnv . to M.keysSet)
    let unboundsExceptRecursive =
          unbounds `S.difference` S.fromList (nameAndExpr ^.. each . _1)

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
    solverResults <- traverse (\cs -> runSolve $ cs <> cs') css

    results       <- for (zip ts solverResults) $ \(t, (subst, unsolved)) -> do
      let preds = subst `apply` toPreds unsolved
      let t'    = subst `apply` t

      --Ideally, do a difference between these lists, but that's too hard for now
      unless (length unsolved == length (toPreds unsolved))
        $ throwError
        $ WrongConstraint unsolved

      pure (subst, preds, t')

    let nameAndResult = zip (nameAndExpr ^.. each . _1) results
    pure $ M.fromList nameAndResult
  TLLetNoBody _ _ -> throwError Weirdness

closeOver :: [Pred] -> Type -> Scheme
closeOver preds =
  normalize (mkSolveEnv ^. classEnv)
    . generalize (fromPred BecauseCloseOver <$> preds) S.empty

extendMonos :: TVar -> Infer a -> Infer a
extendMonos x = local (S.insert x)

extendManyMonos :: [TVar] -> Infer a -> Infer a
extendManyMonos xs = local (<> S.fromList xs)

-- helper constructors
makeFn2 :: Type -> Type -> Type
makeFn2 a b = typeArrow `TAp` a `TAp` b

makeFn3 :: Type -> Type -> Type -> Type
makeFn3 a b c = typeArrow `TAp` a `TAp` makeFn2 b c

makeArrow :: Type -> Type -> Type -> Type
makeArrow x f y = f `TAp` x `TAp` y

-- | Adds a 'Leq t u' constraint for every type 'u' in the assumptions
leq :: Type -> A.Assumption Type -> [Constraint]
leq t = fmap (\x -> CGeq BecauseLeq x t) . A.values

-- | Adds a 'Un' constraint if the name is in assumptions
wkn :: Name -> Type -> A.Assumption Type -> [Constraint]
wkn x t as | x `A.notMember` as = [CUn BecauseWkn t]
           | otherwise          = []

-- | Adds a 'Un' constraint for every type in the assumptions
unrestricted :: A.Assumption Type -> [Constraint]
unrestricted = fmap (CUn BecauseUn) . A.values

-- | Creates a fresh function with some juicy constraints!
--
-- Note: this requires expr only to have better diagnostics!
freshFun :: Expr -> Infer (Type, [Constraint])
freshFun expr = do
  f <- freshType arrowKind
  pure (f, because (BecauseExpr expr) <$> [CFun BecauseFun f])

-- | Take an expression and produce assumptions, result type and constraints to be solved
infer :: Expr -> Infer (A.Assumption Type, Type, [Constraint])
infer expr = case expr of
  Literal cursor typ -> do
    t <- note Weirdness typ
    pure (A.empty, t, [])

  -- A variable x has type t as an assumption  
  Var cursor x -> do
    tv <- freshType StarKind
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
    (tv, a)     <- freshTypeAndTVar StarKind
    (as, t, cs) <- a `extendMonos` infer e

    (f, fPreds) <- freshFun expr -- this is the arrow kind
    let as'   = as `A.remove` x
    -- TODO: this is a bit of a hack, it should be just 'as' but that produces wrong results. this should really be checked!

    let preds = nub $ leq f as' <> wkn x tv as

    pure
      ( as `A.remove` x
      , makeArrow tv f t
      , cs
      <> [ CEq (BecauseExpr e) t' tv | t' <- A.lookup x as ]
      <> fPreds
      <> preds
      )

  App cursor e1 e2 -> do
    (as1, t1, cs1) <- infer e1
    (as2, t2, cs2) <- infer e2
    tv             <- freshType StarKind

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
    tv             <- freshType StarKind
    let preds = nub $ unrestricted (as1 `A.intersection` as2) <> wkn x tv as2 -- Q in paper
    pure
      ( (as1 <> as2) `A.remove` x
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
        tv1      <- freshType StarKind
        tv2      <- freshType StarKind
        resultTv <- freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv typeBool
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpAdd -> do -- TODO: B O I L E R P L A T E
        tv1      <- freshType StarKind
        tv2      <- freshType StarKind
        resultTv <- freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv tv1
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpSub -> do -- TODO: B O I L E R P L A T E
        tv1      <- freshType StarKind
        tv2      <- freshType StarKind
        resultTv <- freshType StarKind

        let f = makeFn3 tv1 tv2 resultTv

        pure
          ( A.empty
          , f
          , [ CEq (BecauseExpr expr) resultTv tv1
            , CEq (BecauseExpr expr) tv1      tv2
            ]
          )

      BinOpMul -> do
        tv1      <- freshType StarKind
        tv2      <- freshType StarKind
        resultTv <- freshType StarKind

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
        tv  <- freshType StarKind
        tv' <- freshType StarKind
        let resultType = typePtrOf tv'
        let f          = makeFn2 tv resultType

        pure (A.empty, f, [CEq (BecauseExpr expr) tv tv'])
      UnOpDeref -> do
        tv         <- freshType StarKind
        resultType <- freshType StarKind
        let f = makeFn2 (typePtrOf tv) resultType

        pure (A.empty, f, [CEq (BecauseExpr expr) tv resultType])
      other -> error $ "not implemented yet: " <> show other
    BuiltinMemberRef -> error "not implemented yet"
    BuiltinArraySubscript clangArrayTyp clangSubscriptTyp -> do
      array     <- freshType (ArrowKind StarKind StarKind)
      arrayElem <- freshType StarKind
      subscript <- freshType StarKind
      result    <- freshType StarKind

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
      tv    <- freshType StarKind

      someA <- freshType StarKind
      let somePointer = typePtrOf someA

      pure (A.empty, tv, [CEq (BecauseExpr expr) tv somePointer])
    BuiltinNew typ -> do
      tv         <- freshType StarKind
      resultType <- freshType StarKind

      let f = makeFn2 tv resultType

      pure (A.empty, f, [CEq (FromClang typ expr) typ resultType])

-- | This is the top-level function that should be used for inferring a type of something
inferTop :: InferEnv -> [TopLevel] -> Either InferError InferEnv
inferTop env []         = Right env
inferTop env (tl : tls) = case inferTopLevel env tl of
  Left  err -> Left err
  Right m   -> inferTop (env & typeEnv <>~ m) tls

-- | Attempt to convert the type scheme into a normal(ish) format  
normalize :: ClassEnv -> Scheme -> Scheme
normalize env (Forall origVars (origPreds :=> origBody)) = Forall
  (M.elems sub)
  ((properSub `apply` normpreds (S.fromList preds)) :=> normtype body)
 where
  Forall _ (preds :=> body) = simplifyScheme
    $ Forall origVars (normpreds (S.fromList origPreds) :=> origBody)
  bodyFV     = ftv body
  usefulVars = S.toList $ S.fromList origVars `S.intersection` bodyFV
  sub        = M.fromList $ (\(old@(TV _ k), n) -> (old, TV n k)) <$> zip
    usefulVars
    (Name . ("t" <>) . Text.pack . show <$> [1 ..])

  properSub = Subst $ M.map TVar sub

  normtype :: Type -> Type
  normtype (TAp a b)  = normtype a `TAp` normtype b
  normtype con@TCon{} = con
  normtype (TVar a)   = case a `M.lookup` sub of
    Just x  -> TVar x
    Nothing -> error "tv not in signature"

  normpreds :: S.Set Pred -> [Pred]
  normpreds ps = S.toList $ S.difference ps env

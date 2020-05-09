{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Solve where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                      ( delete
                                                , find
                                                , nub
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as S
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Type
import           MonadFresh
import           Unify
import           Core.Located
import           Core.Syntax                    ( Expr )
import           Debug.Trace                    ( traceShow )

data Reason = BecauseExpr Expr
            | PairedAssumption Name Type
            | Simplified Reason
            | Generalized Reason
            | Instantiated Reason
            | BecauseCloseOver
            | BecauseInstantiate
            | BecauseFun
            | BecauseWkn
            | BecauseUn
            | BecauseLeq
            | FromClang Type Expr
            | CombinedReason Reason Reason
            deriving stock (Eq, Show)

instance Pretty Reason where
  pretty (BecauseExpr expr) =
    "from expression at" <+> prettyLocation (loc expr)
  pretty (PairedAssumption n t) =
    "from assumption:" <+> pretty n <+> "::" <+> pretty t
  pretty (Simplified r)       = "from simplification of:" <+> PP.indent 4 (pretty r)
  pretty (Generalized r)      = "from generalization of:" <+> PP.indent 4 (pretty r)
  pretty (Instantiated r)     = "from instantiation of:" <+> PP.indent 4 (pretty r)
  pretty BecauseCloseOver     = "from closing over a type"
  pretty BecauseInstantiate   = "from instantiation of a predicate"
  pretty BecauseWkn           = "from a [Wkn] rule"
  pretty BecauseUn            = "from a [Un] rule"
  pretty BecauseLeq           = "from a [Leq] rule"
  pretty BecauseFun           = "from a [Fun] rule"
  pretty (FromClang typ expr) = PP.align $ PP.sep
    [ "from Clang got type:" <+> pretty typ
    , PP.indent 4 $ "of expression at" <+> prettyLocation (loc expr)
    ]
  pretty (CombinedReason first second) =
    PP.align $ PP.sep [pretty first, pretty second]

because :: Reason -> Constraint -> Constraint
because r' c = c & reasonL %~ (\r -> CombinedReason r r')

data Constraint = CEq Reason Type Type
                | CExpInst Reason Type Scheme
                | CImpInst Reason Type (S.Set TVar) Type
--                | CCtor Reason Name Type -- new addition: `is constructor of`
                | CIn Reason Name (NonEmpty Type)
                deriving stock (Show)

reasonL :: Lens' Constraint Reason
reasonL = lens getter (flip setter)
 where
  getter = \case
    CEq      r _ _   -> r
    CExpInst r _ _   -> r
    CImpInst r _ _ _ -> r
    CIn r _ _        -> r
  setter :: Reason -> Constraint -> Constraint
  setter r' = \case
    CEq      _ a b         -> CEq r' a b
    CExpInst _ t s         -> CExpInst r' t s
    CImpInst _ t1 monos t2 -> CImpInst r' t1 monos t2
    CIn _ n ts             -> CIn r' n ts

-- | Equality disregards the reason
-- which is just for diagnostics/nice output
instance Eq Constraint where
  CEq _ t1 u1         == CEq _ t2 u2         = t1 == t2 && u1 == u2
  CImpInst _ t1 m1 u1 == CImpInst _ t2 m2 u2 = t1 == t2 && m1 == m2 && u1 == u2
  CExpInst _ t1 s1    == CExpInst _ t2 s2    = t1 == t2 && s1 == s2
  CIn      _ n1 ts1   == CIn      _ n2 ts2   = n1 == n2 && ts1 == ts2
  _                   == _                   = False

instance Pretty Constraint where
  pretty (CEq      _ x y) = pretty x <+> "~" <+> pretty y
  pretty (CExpInst _ t s) = pretty t <+> "≼" <+> pretty s
  pretty (CImpInst _ t1 mono t2) =
    pretty t1 <+> "≼{" <+> pretty (S.toList mono) <+> "}" <+> pretty t2
--  pretty (CCtor _ name t ) = pretty name <+> "c" <+> pretty t
  pretty (CIn _ name ts) = pretty name <+> PP.hsep (NE.toList $ pretty <$> ts)

instance ActiveTypeVars Constraint where
  atv (CEq _ t1 t2) = ftv t1 `S.union` ftv t2
  atv (CImpInst _ t1 monos t2) =
    ftv t1 `S.union` (ftv monos `S.intersection` ftv t2)
  atv (CExpInst _ t s ) = ftv t `S.union` ftv s
--  atv (CCtor    _ n t ) = ftv t
  atv (CIn      _ _ ts) = foldr1 S.union (ftv <$> ts) -- S.empty  -- this should be correct as we don't really work with these

instance Substitutable Constraint where
  apply sub (CEq      r t1 t2) = CEq r (apply sub t1) (apply sub t2)
  apply sub (CExpInst r t  s ) = CExpInst r (apply sub t) (apply sub s)
  apply sub (CImpInst r t1 monos t2) =
    CImpInst r (apply sub t1) (apply sub monos) (apply sub t2)
--  apply sub (CCtor r name t ) = CCtor r name (apply sub t)
  apply sub (CIn r name ts) = CIn r name (apply sub ts) -- the fmap shouldn't be needed, just making sure!

data SolveError = SolveError Reason UnificationError -- for now, anyways...

instance Pretty SolveError where
  pretty (SolveError r x) =
    PP.align $ PP.vsep [pretty x, "Because:" <+> pretty r]

type Solve a = ReaderT SolveEnv (FreshT Name (Except SolveError)) a

type ClassEnv = S.Set Pred
type CtorEnv = M.Map Name Scheme
data SolveEnv = SolveEnv { _classEnv :: ClassEnv, _ctorEnv :: CtorEnv } deriving stock (Show, Ord, Eq)
makeLenses ''SolveEnv

runSolveT
  :: FreshState Name -> Solve a -> Either SolveError (a, FreshState Name)
runSolveT freshSt m = runExcept $ runFreshT (runReaderT m mkSolveEnv) freshSt

mkSolveEnv :: SolveEnv
mkSolveEnv = SolveEnv { _classEnv = mkClassEnv, _ctorEnv = mkCtorEnv }
 where
  mkClassEnv = S.fromList
    [ PNum typeInt
    , PUn typeChar
    , PUn typeInt
    , PUn typeBool
    , PUn typeUnArrow
    , PFun typeArrow
    , PFun typeLinArrow
    , PFun typeUnArrow
    ]
  mkCtorEnv = M.fromList [("Pair", pairCtor), ("Pointed", pointedCtor)]

pairCtor :: Scheme
pairCtor = Forall
  [aVar, bVar]
  (   []
  :=> (     typeArrow
      `TAp` a
      `TAp` (typeUnArrow `TAp` b `TAp` (pairType `TAp` a `TAp` b))
      )
  )
 where
  (aVar, bVar) = (TV "a" StarKind, TV "b" StarKind)
  (a   , b   ) = (TVar aVar, TVar bVar)
  pairType     = TCon (TC "Pair" arrowKind)

pointedCtor :: Scheme
pointedCtor = Forall
  [aVar]
  ([] :=> (typeUnArrow `TAp` a `TAp` (pointedType `TAp` a)))
 where
  (aVar, bVar) = (TV "a" StarKind, TV "b" StarKind)
  (a   , b   ) = (TVar aVar, TVar bVar)
  pointedType  = TCon (TC "Pointed" (ArrowKind StarKind StarKind))

-- these patterns are bidirectional in this weird way
-- only because of using OverloadedLists
--
-- otherwise they should be bidirectional automatically with `pattern CX x = CIn "X" [x]`
-- sigh.
pattern CFun :: Reason -> Type -> Constraint
pattern CFun r x <- CIn r "Fun" [x] where CFun r x = CIn r "Fun" [x]
pattern CUn :: Reason -> Type -> Constraint
pattern CUn r x <- CIn r "Un" [x] where CUn r x = CIn r "Un" [x]
pattern CGeq :: Reason -> Type -> Type -> Constraint
pattern CGeq r x y <- CIn r "Geq" [x, y] where CGeq r x y = CIn r "Geq" [x, y]

toPreds :: [Constraint] -> [Pred]
toPreds cs = [ IsIn n ts | CIn _ n ts <- cs ]
fromPred :: Reason -> Pred -> Constraint
fromPred r (IsIn n ts) = CIn r n ts

-- | Takes a class environment, constraint being simplified,
-- rest of constraints for context and returns new contexts being created from the simplified context
simplify :: ClassEnv -> Constraint -> [Constraint] -> [Constraint]
-- ^ this could also return Maybe and then use `catMaybes`
-- TODO: we should verify that 'f' is indeed a function (there exists a 'CIn "Fun" [f]' in here
-- TODO: check that these rules actually make sense!
simplify env (CGeq r other (f `TAp` _ `TAp` _)) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) other f]
simplify env (CGeq r (f `TAp` _ `TAp` _) other) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) f other]
simplify env c@(CIn _ n ts) cs
  | IsIn n ts `S.member` env = []
  | otherwise                = simplifyGeq (findConstraint env cs) c
simplify _ c _ = [c]

findConstraint :: ClassEnv -> [Constraint] -> Constraint -> Bool
findConstraint env (c : cs) x | c == x    = True
                              | otherwise = findConstraint env cs x
findConstraint env [] (CIn _ n ts) = IsIn n ts `S.member` env
findConstraint _   _  _            = False

-- | If we see constraint of type (Un t, t >= u),
-- then we definitively throw the 'Un t' constraint away
-- since we're not learning anything new!
simplifyGeq :: (Constraint -> Bool) -> Constraint -> [Constraint]
simplifyGeq p c@(CGeq r a _) | p (CUn r a) = traceShow ("deleting", pretty c) []
simplifyGeq p c@(CGeq r a _) | p (CFun r a) && a == typeLinArrow = traceShow ("deleting", pretty c) []
simplifyGeq _ c                            = [c]

simplifyMany :: ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs = chooseOne cs >>= uncurry (simplify e)

-- Solving:
solve :: [Constraint] -> Solve (Subst, [Constraint])
solve [] = pure (emptySubst, [])
solve cs
  | all (not . solvable) (chooseOne cs) = pure (emptySubst, cs)
  | otherwise = {-traceShow ("constraints left : ", pretty cs) $-}
                do
    env <- view classEnv
    solve' $ nextSolvable $ simplifyMany env cs

solve' :: (Constraint, [Constraint]) -> Solve (Subst, [Constraint])
solve' (CEq r t1 t2, cs) = do
  sub1             <- addReason r $ t1 `unifies` t2
  (sub2, unsolved) <- solve (sub1 `apply` cs)
  pure (sub2 `compose` sub1, (sub2 `compose` sub1) `apply` unsolved)
solve' (CImpInst r t1 monos t2, cs) = do
  let t2' = generalize cs monos t2
  solve (CExpInst (Generalized r) t1 t2' : cs)
solve' (CExpInst r t s, cs) = do
  -- [ this doesn't work! it discards a lot of the information we need! ]  
  -- (s' , unsolved ) <- instantiate s
  -- (sub, unsolved') <- solve (CEq t s' : cs)
  -- return $ traceShow ("created-sub: ", sub) (sub, unsolved ++ unsolved')
  (s' , preds   ) <- instantiate s
  (sub, unsolved) <- solve $ CEq (Instantiated r) t s' : preds <> cs
  pure (sub, unsolved)
{-solve' (CCtor _ n t, cs) = do
  env <- view ctorEnv
  case M.lookup n env of
    Nothing   -> throwError $ ConstructorNotFound n
    Just ctor -> solve (CExpInst t ctor : cs)-}
solve' (CIn{}, _) =
  error "This should never happen, such a constraint is impossible!"

addReason :: Reason -> Except UnificationError a -> Solve a
addReason r x = case runExcept x of
  Left  unificationErr -> throwError $ SolveError r unificationErr
  Right result         -> pure result

solvable :: (Constraint, [Constraint]) -> Bool
solvable (CEq{}     , _) = True
solvable (CExpInst{}, _) = True
solvable (CImpInst _ _ monos t2, cs) =
  S.null $ (ftv t2 `S.difference` monos) `S.intersection` atv cs
--solvable (CCtor{}, _) = True
solvable (CIn{}, _) = False

-- this is probably very inefficient, but eh
chooseOne :: Eq a => [a] -> [(a, [a])]
chooseOne xs = [ (x, x `delete` xs) | x <- xs ]

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs =
  {-trace ("all solvable: " <> (show $ PP.group $ PP.hsep $ pretty <$> allSolvable xs))
      $-}
                  fromJust . find solvable . chooseOne $ xs
  where allSolvable zs = let ys = chooseOne zs in [ x | x <- ys, solvable x ]

instantiate :: Scheme -> Solve (Type, [Constraint])
instantiate (Forall xs qt) = do
  xs' <- traverse (\(TV _ k) -> freshType k) xs
  let sub         = Subst $ M.fromList $ zip xs xs'
      preds :=> t = sub `apply` qt

  pure (t, fromPred BecauseInstantiate <$> preds)

-- TODO: This function is a bit incorrect, see below
generalize :: [Constraint] -> S.Set TVar -> Type -> Scheme
generalize unsolved free t = Forall (S.toList tyVars) (preds :=> t)
 where
  tyVars = ftv t `S.difference` free
  preds  = toPreds $ nub unsolved
  -- preds  = filter (\(IsIn _ xs) -> any (`isIn` tyVars) xs) (toPreds unsolved) -- This for example discharges any (Un $ConcreteType) even when it's really not true! That's a real problem! TODO TODO TODO
  -- isIn (TVar x) xs = x `S.member` xs
  -- isIn _        _  = False

freshType :: MonadFresh Name m => Kind -> m Type
freshType kind = do
  name <- fresh
  let tv = TV name kind
  pure $ TVar tv

freshTypeAndTVar :: MonadFresh Name m => Kind -> m (Type, TVar)
freshTypeAndTVar kind = do
  name <- fresh
  let tv = TV name kind
  pure (TVar tv, tv)

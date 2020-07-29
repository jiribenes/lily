{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Solve where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                      ( delete
                                                , find
                                                , nub
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as S
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Control.Monad.Fresh
import           Name
import           Type.Constraint
import           Type.Type
import           Type.Unify
import           Type.Fresh
import qualified Type.Class                    as C

-- | An error during constraint solving process is a
-- error from the unification phase together with a 'Reason'
data SolveError = SolveError Reason UnificationError
    deriving stock (Eq, Show)

instance Pretty SolveError where
  pretty (SolveError r x) =
    PP.align $ PP.vsep [pretty x, "Because:" <+> pretty r]

-- | The Solve monad allows the user to read from 'SolveEnv',
-- generate fresh 'Name's and throw 'SolveError's.
type Solve a = ReaderT SolveEnv (FreshT Name (Except SolveError)) a

-- | A 'SolveEnv' is just a 'C.ClassEnv'
data SolveEnv = SolveEnv { _solveClassEnv :: C.ClassEnv}
makeClassy ''SolveEnv

-- | Runs a solver computation with all its effects,
-- essentially just unwraps a 'Solve'.
runSolveT
  :: C.ClassEnv
  -> FreshState Name
  -> Solve a
  -> Either SolveError (a, FreshState Name)
runSolveT ce freshSt m =
  runExcept $ runFreshT (runReaderT m (SolveEnv ce)) freshSt

-- | Converts a list of predicate constraints to a predicate
toPreds :: [Constraint] -> [Pred]
toPreds cs = [ IsIn n ts | CIn _ n ts <- cs ]

-- | Creates a predicate from a constraint
fromPred :: Reason -> Pred -> Constraint
fromPred r (IsIn n ts) = CIn r n ts

-- | Takes a class environment, constraint being simplified,
-- rest of constraints for context and returns new contexts being created from the simplified context
simplify :: C.ClassEnv -> Constraint -> [Constraint] -> [Constraint]
simplify env (CGeq r other (f `TAp` _ `TAp` _)) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) other f]
simplify env (CGeq r (f `TAp` _ `TAp` _) other) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) f other]
simplify env (CUn r (f `TAp` _ `TAp` _)) cs
  | findConstraint env cs (CFun r f) = [CUn (Simplified r) f]
  | f == typeUnArrow                 = []
simplify env (CUn r (t `TAp` _)) cs | t == typeLRef = []
simplify env c@(CIn _ n ts) cs = case C.substPred env $ IsIn n ts of
  Nothing        -> simplifyGeq (findConstraint env cs) c
  Just (Subst s) -> case M.toList s of
    []       -> []
    [(a, v)] -> [CEq BecauseFunDep (TVar a) v]
    _        -> error "impossible"
simplify _ c _ = [c]

-- | Find the given constraint either among the other constraints or in the 'ClassEnv'
findConstraint :: C.ClassEnv -> [Constraint] -> Constraint -> Bool
findConstraint env (c : cs) x | c == x    = True
                              | otherwise = findConstraint env cs x
findConstraint env [] (CIn _ n ts) = IsIn n ts `C.member` env
findConstraint _   _  _            = False

-- | Simplifies @a >= b@ constraints
--
-- If we see constraint of type (Un t, t >= u),
-- then we definitively throw the 'Un t' constraint away
-- since we're not learning anything new!
simplifyGeq :: (Constraint -> Bool) -> Constraint -> [Constraint]
simplifyGeq p c@(CGeq r a b) | p (CUn r a) = []

-- Follows from entailment rule:
-- | Fun b, NOT (Un b), a ~ linArrow
-- |-------------------------------
-- |      b ~ unArrow
-- |
simplifyGeq p c@(CGeq r a b)
  | not (p (CUn r b)) && p (CFun r b) && p (CFun r a) && a == typeLinArrow
  = [CEq r b typeLinArrow]

-- Follows from entailment rule:
-- |
-- |----------------
-- | a >= linArrow
-- |
simplifyGeq p c@(CGeq r _ b) | p (CFun r b) && b == typeLinArrow = []
simplifyGeq _ c = [c]

-- | Simplifies constraints until a fix-point is reached
-- where no more constraints can be simplified.
-- Note that there is always at least one simplification
-- and the number of constraints is non-increasing,
-- which results in totality of this function.
simplifyMany :: C.ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs | cs == cs' = cs
                  | otherwise = simplifyMany e cs'
  where cs' = chooseOne cs >>= uncurry (simplify e)

-- | Solves a single constraint from a multiset of constraints,
-- producing a substitution
solve :: [Constraint] -> Solve (Subst, [Constraint])
solve [] = pure (emptySubst, [])
solve cs
  | all (not . solvable) (chooseOne cs) = pure (emptySubst, cs)
  | otherwise = do
    env <- view solveClassEnv
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
  (s', preds) <- instantiate s
  let cs' = fromPred BecauseInstantiate <$> preds
  (sub, unsolved) <- solve $ CEq (Instantiated s s' r) t s' : cs' <> cs
  pure (sub, unsolved)
solve' (CIn{}, _) =
  error
    "This should never happen, such a constraint is unsolvable and shouldn't get to `solve'`!"

-- | Adds a 'Reason' to a 'UnificationError'
addReason :: Reason -> Except UnificationError a -> Solve a
addReason r x = case runExcept x of
  Left  unificationErr -> throwError $ SolveError r unificationErr
  Right result         -> pure result

-- | Checks if a constraint is solvable
solvable :: (Constraint, [Constraint]) -> Bool
solvable (CEq{}     , _) = True
solvable (CExpInst{}, _) = True
solvable (CImpInst _ _ monos t2, cs) =
  S.null $ (ftv t2 `S.difference` ftv monos) `S.intersection` atv cs
solvable (CIn{}, _) = False

-- | Enumerates all non-deterministic choices for
-- pluicking a single element from a list.
--
-- This is probably super inefficient, but it does not matter.
chooseOne :: Eq a => [a] -> [(a, [a])]
chooseOne xs = [ (x, x `delete` xs) | x <- xs ]

-- | Plucks a solvable constraint from the list of constraints
nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust . find solvable . chooseOne $ xs

-- | Generalizes a type (P :=> \tau) into a polytype under a context
generalize :: [Constraint] -> S.Set Type -> Type -> Scheme
generalize unsolved free t = Forall (S.toList tyVars) (preds :=> t)
 where
  tyVars = ftv t `S.difference` ftv free
  preds  = toPreds $ nub unsolved

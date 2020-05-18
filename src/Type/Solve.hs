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
import           Debug.Trace                    ( traceShow )

import           Control.Monad.Fresh
import           Name
import           Type.Constraint
import           Type.Type
import           Type.Unify
import qualified Type.Class                    as C

data SolveError = SolveError Reason UnificationError -- for now, anyways...
    deriving stock (Eq, Show)

instance Pretty SolveError where
  pretty (SolveError r x) =
    PP.align $ PP.vsep [pretty x, "Because:" <+> pretty r]

type Solve a = ReaderT SolveEnv (FreshT Name (Except SolveError)) a

data SolveEnv = SolveEnv { _solveClassEnv :: C.ClassEnv}
makeClassy ''SolveEnv

runSolveT
  :: C.ClassEnv
  -> FreshState Name
  -> Solve a
  -> Either SolveError (a, FreshState Name)
runSolveT ce freshSt m =
  runExcept $ runFreshT (runReaderT m (SolveEnv ce)) freshSt

toPreds :: [Constraint] -> [Pred]
toPreds cs = [ IsIn n ts | CIn _ n ts <- cs ]
fromPred :: Reason -> Pred -> Constraint
fromPred r (IsIn n ts) = CIn r n ts

-- | Takes a class environment, constraint being simplified,
-- rest of constraints for context and returns new contexts being created from the simplified context
simplify :: C.ClassEnv -> Constraint -> [Constraint] -> [Constraint]
simplify env (CGeq r other (f `TAp` _ `TAp` _)) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) other f]
simplify env (CGeq r (f `TAp` _ `TAp` _) other) cs
  | findConstraint env cs (CFun r f) = [CGeq (Simplified r) f other]
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

-- | If we see constraint of type (Un t, t >= u),
-- then we definitively throw the 'Un t' constraint away
-- since we're not learning anything new!
simplifyGeq :: (Constraint -> Bool) -> Constraint -> [Constraint]
simplifyGeq p c@(CGeq r a b) | p (CUn r a) = traceShow ("deleting", pretty c) []

-- Follows from entailment rule:
-- | Fun b, NOT (Un b), a ~ linArrow
-- |-------------------------------
-- |      b ~ unArrow
-- |
simplifyGeq p c@(CGeq r a b)
  | not (p (CUn r b)) && p (CFun r b) && p (CFun r a) && a == typeLinArrow
  = traceShow ("replacing", pretty c) ([CEq r b typeLinArrow] :: [Constraint])

-- Follows from entailment rule:
-- |
-- |----------------
-- | a >= linArrow
-- |
simplifyGeq p c@(CGeq r _ b) | p (CFun r b) && b == typeLinArrow =
  traceShow ("deleting", pretty c) []
simplifyGeq _ c = [c]

-- | Simplifies constraints until a fix-point is reached
-- where no more constraints can be simplified
-- 
-- TODO: This should ideally also have a counter so
-- we can't loop infinitely (but we really shouldn't!)
simplifyMany :: C.ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs | cs == cs' = cs
                  | otherwise = simplifyMany e cs' --TODO: simplification is disabled for the time-being
  where cs' = chooseOne cs >>= uncurry (simplify e)

-- Solving:
solve :: [Constraint] -> Solve (Subst, [Constraint])
solve [] = pure (emptySubst, [])
solve cs
  | all (not . solvable) (chooseOne cs) = pure (emptySubst, cs)
  | otherwise = {-traceShow ("constraints left : ", pretty cs) $-}
                do
    env <- view solveClassEnv
    solve' $ nextSolvable $ simplifyMany env cs

solve' :: (Constraint, [Constraint]) -> Solve (Subst, [Constraint])
solve' (CEq r t1 t2, cs) = do
  sub1             <- addReason r $ t1 `unifies` t2
  (sub2, unsolved) <- solve (sub1 `apply` cs)
  pure (sub2 `compose` sub1, (sub2 `compose` sub1) `apply` unsolved)
solve' (CImpInst r t1 monos t2, cs) = do
  -- Note: We're not doing Morris' improving substitution here,
  -- but we should do it at the end! 
  -- TODO 
  -- Currently this is in Type.Simplify which gets run only on already finished, generalized types
  let t2' = generalize cs monos t2
  solve (CExpInst (Generalized r) t1 t2' : cs)
solve' (CExpInst r t s, cs) = do
  (s' , preds   ) <- instantiate s
  (sub, unsolved) <- solve $ CEq (Instantiated s s' r) t s' : preds <> cs
  pure (sub, unsolved)
solve' (CIn{}, _) =
  error
    "This should never happen, such a constraint is unsolvable and shouldn't get to `solve'`!"

addReason :: Reason -> Except UnificationError a -> Solve a
addReason r x = case runExcept x of
  Left  unificationErr -> throwError $ SolveError r unificationErr
  Right result         -> pure result

solvable :: (Constraint, [Constraint]) -> Bool
solvable (CEq{}     , _) = True
solvable (CExpInst{}, _) = True
solvable (CImpInst _ _ monos t2, cs) =
  S.null $ (ftv t2 `S.difference` ftv monos) `S.intersection` atv cs
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

generalize :: [Constraint] -> S.Set Type -> Type -> Scheme
generalize unsolved free t = Forall (S.toList tyVars) (preds :=> t)
 where
  tyVars = ftv t `S.difference` ftv free
  preds  = toPreds $ nub unsolved

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

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

-- | Find the given constraint either among the other constraints or in the 'ClassEnv'
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
simplifyGeq p c@(CGeq r a _) | p (CFun r a) && a == typeLinArrow =
  traceShow ("deleting", pretty c) []
simplifyGeq _ c = [c]

-- | Simplifies constraints until a fix-point is reached
-- where no more constraints can be simplified
-- 
-- TODO: This should ideally also have a counter so
-- we can't loop infinitely (but we really shouldn't!)
simplifyMany :: ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs | cs == cs' = cs
                  | otherwise = simplifyMany e cs'
  where
    cs' = chooseOne cs >>= uncurry (simplify e)

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

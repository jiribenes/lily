{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Solve where

import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Lens


import           Data.List                      ( nub
                                                , intersect
                                                , delete
                                                , find
                                                , intersperse
                                                )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T

import           Type
import           MonadFresh
import           Unify

import           Debug.Trace

data Constraint = CEq Type Type
                | CExpInst Type Scheme
                | CImpInst Type (S.Set TVar) Type
                | CCtor Name Type -- new addition: `is constructor of`
                | CIn Name [Type]
                deriving (Ord, Eq)

instance Show Constraint where
  show (CEq      x y) = show x <> " ~ " <> show y
  show (CExpInst t s) = show t <> " ≼ " <> show s
  show (CImpInst t1 mono t2) =
    show t1 <> " ≼{" <> show (S.toList mono) <> "} " <> show t2
  show (CCtor name t ) = show name <> " c " <> show t
  show (CIn   name ts) = show name <> " " <> unwords (show <$> ts)

instance ActiveTypeVars Constraint where
  atv (CEq t1 t2) = ftv t1 `S.union` ftv t2
  atv (CImpInst t1 monos t2) =
    ftv t1 `S.union` (ftv monos `S.intersection` ftv t2)
  atv (CExpInst t s ) = ftv t `S.union` ftv s
  atv (CCtor    n t ) = ftv t
  atv (CIn      _ ts) = S.empty  -- this should be correct as we don't really work with these

instance Substitutable Constraint where
  apply sub (CEq      t1 t2) = CEq (apply sub t1) (apply sub t2)
  apply sub (CExpInst t  s ) = CExpInst (apply sub t) (apply sub s)
  apply sub (CImpInst t1 monos t2) =
    CImpInst (apply sub t1) (apply sub monos) (apply sub t2)
  apply sub (CCtor name t ) = CCtor name (apply sub t)
  apply sub (CIn   name ts) = CIn name (apply sub ts) -- the fmap shouldn't be needed, just making sure!

type SolveError = UnificationError -- for now, anyways...
type Solve a = ReaderT SolveEnv (FreshT Name (Except SolveError)) a

type ClassEnv = S.Set Pred
type CtorEnv = M.Map Name Scheme
data SolveEnv = SolveEnv { _classEnv :: ClassEnv, _ctorEnv :: CtorEnv } deriving (Show, Ord, Eq)
makeLenses ''SolveEnv

runSolveT
  :: FreshState Name -> Solve a -> Either SolveError (a, FreshState Name)
runSolveT freshSt m = runExcept $ runFreshT (runReaderT m mkSolveEnv) freshSt

mkSolveEnv :: SolveEnv
mkSolveEnv = SolveEnv { _classEnv = mkClassEnv, _ctorEnv = mkCtorEnv }
 where
  mkClassEnv = S.fromList
    [ predNum typeInt
    , predUn typeChar
    , predUn typeInt
    , predUn typeBool
    , predUn typeUnArrow
    , predFun typeArrow
    , predFun typeLinArrow
    , predFun typeUnArrow
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


pattern CFun :: Type -> Constraint
pattern CFun x = CIn "Fun" [x]
pattern CUn :: Type -> Constraint
pattern CUn x = CIn "Un" [x]
pattern CGeq :: Type -> Type -> Constraint
pattern CGeq x y = CIn "Geq" [x, y]
pattern CMut :: Type -> Constraint
pattern CMut x = CIn "Mut" [x]

toPreds cs = [ IsIn n ts | CIn n ts <- cs ]
fromPred (IsIn n ts) = CIn n ts

simplify :: ClassEnv -> Constraint -> [Constraint]
-- ^ this could also return Maybe and then use `catMaybes`
-- TODO: we should verify that 'f' is indeed a function (there exists a 'CIn "Fun" [f]' in here
-- TODO: check that these rules actually make sense!
simplify _ (CGeq other               (f `TAp` _ `TAp` _)) = [CGeq other f]
simplify _ (CGeq (f `TAp` _ `TAp` _) other              ) = [CGeq f other]
simplify env c@(CIn n ts) | IsIn n ts `S.member` env = []
                          | otherwise                = [c]
simplify _ c = [c]

simplifyMany :: ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs = cs >>= simplify e

-- Solving:
solve :: [Constraint] -> Solve (Subst, [Constraint])
solve [] = pure (emptySubst, [])
solve cs
  | all (not . solvable) (chooseOne cs) = pure (emptySubst, cs)
  | otherwise = traceShow ("constraints left : ", cs) $ do
    env <- view classEnv
    solve' $ nextSolvable $ simplifyMany env cs

solve' :: (Constraint, [Constraint]) -> Solve (Subst, [Constraint])
solve' (CEq t1 t2, cs) = do
  sub1             <- t1 `unifies` t2
  (sub2, unsolved) <- solve (sub1 `apply` cs)
  pure (sub2 `compose` sub1, (sub2 `compose` sub1) `apply` unsolved)
solve' (CImpInst t1 monos t2, cs) = do
  let t2' = generalize cs monos t2
  solve (CExpInst t1 t2' : cs)
solve' (CExpInst t s, cs) = do
  -- [ this doesn't work! it discards a lot of the information we need! ]  
  -- (s' , unsolved ) <- instantiate s
  -- (sub, unsolved') <- solve (CEq t s' : cs)
  -- return $ traceShow ("created-sub: ", sub) (sub, unsolved ++ unsolved')
  (s' , preds   ) <- instantiate s
  (sub, unsolved) <- solve $ (CEq t s' : preds) <> cs
  pure (sub, unsolved)
solve' (CCtor n t, cs) = do
  env <- view ctorEnv
  case M.lookup n env of
    Nothing   -> throwError $ ConstructorNotFound n
    Just ctor -> solve (CExpInst t ctor : cs)
solve' (CIn{}, _) =
  error "This should never happen, such a constraint is impossible!"

solvable :: (Constraint, [Constraint]) -> Bool
solvable (CEq{}     , _) = True
solvable (CExpInst{}, _) = True
solvable (CImpInst _ monos t2, cs) =
  S.null $ (ftv t2 `S.difference` monos) `S.intersection` atv cs
solvable (CCtor{}, _) = True
solvable (CIn{}  , _) = False

-- this is probably very inefficient, but eh
chooseOne :: Eq a => [a] -> [(a, [a])]
chooseOne xs = [ (x, x `delete` xs) | x <- xs ]

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs =
  trace ("all solvable: " <> unlines (show <$> allSolvable xs))
    $ fromJust
    . find solvable
    . chooseOne
    $ xs
  where allSolvable zs = let ys = chooseOne zs in [ x | x <- ys, solvable x ]

instantiate :: Scheme -> Solve (Type, [Constraint])
instantiate (Forall xs qt) = do
  xs' <- traverse (\(TV _ k) -> freshType k) xs
  let sub         = Subst $ M.fromList $ zip xs xs'
      preds :=> t = sub `apply` qt
  pure (t, fromPred <$> preds)

-- TODO: This function is a bit incorrect, see below
generalize :: [Constraint] -> S.Set TVar -> Type -> Scheme
generalize unsolved free t =
  traceShow (("tyVars", tyVars), ("preds", preds))
    $ Forall (S.toList tyVars) (preds :=> t)
 where
  tyVars = ftv t `S.difference` free
  preds  = filter (\(IsIn _ xs) -> any (`isIn` tyVars) xs) (toPreds unsolved) -- This for example discharges any (Un $ConcreteType) even when it's really not true! That's a real problem! TODO TODO TODO
  isIn (TVar x) xs = x `S.member` xs
  isIn _        _  = False

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

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

data Reason = BecauseExpr Expr
            | PairedAssumption Name Type
            | Simplified Reason
            | Generalized Reason
            | Instantiated Reason
            | FromPredicate
            | BecauseWkn
            | BecauseUn
            | BecauseLeq
            | FromClang Type Expr
            deriving stock (Eq, Show)

instance Pretty Reason where
  pretty (BecauseExpr expr) =
    "from expression at" <+> prettyLocation (loc expr)
  pretty (PairedAssumption n t) =
    "from assumption:" <+> pretty n <+> "::" <+> pretty t
  pretty (Simplified r) = "from simplification of:" <+> PP.indent 4 (pretty r)
  pretty (Generalized r) = "from generalization of:" <+> PP.indent 4 (pretty r)
  pretty (Instantiated r) = "from instantiation of:" <+> PP.indent 4 (pretty r)
  pretty FromPredicate = "from a predicate"
  pretty BecauseWkn = "from a [Wkn] rule"
  pretty BecauseUn = "from a [Un] rule"
  pretty BecauseLeq = "from a [Leq] rule"
  pretty (FromClang typ expr) = PP.align $ PP.sep
    [ "from Clang got type:" <+> pretty typ
    , "from expression at" <+> prettyLocation (loc expr)
    ]

data Constraint = CEq Reason Type Type
                | CExpInst Reason Type Scheme
                | CImpInst Reason Type (S.Set TVar) Type
--                | CCtor Reason Name Type -- new addition: `is constructor of`
                | CIn Reason Name (NonEmpty Type)
                deriving stock (Show, Eq)

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
fromPred :: Pred -> Constraint
fromPred (IsIn n ts) = CIn FromPredicate n ts

simplify :: ClassEnv -> Constraint -> [Constraint]
-- ^ this could also return Maybe and then use `catMaybes`
-- TODO: we should verify that 'f' is indeed a function (there exists a 'CIn "Fun" [f]' in here
-- TODO: check that these rules actually make sense!
simplify _ (CGeq r other (f `TAp` _ `TAp` _)) = [CGeq (Simplified r) other f]
simplify _ (CGeq r (f `TAp` _ `TAp` _) other) = [CGeq (Simplified r) f other]
simplify env c@(CIn _ n ts) | IsIn n ts `S.member` env = []
                            | otherwise                = [c]
simplify _ c = [c]

simplifyMany :: ClassEnv -> [Constraint] -> [Constraint]
simplifyMany e cs = cs >>= simplify e

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
  pure (t, fromPred <$> preds)

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

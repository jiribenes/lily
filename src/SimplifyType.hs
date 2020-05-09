-- | TODO: 
-- * remove satisfied predicates (somehow?)
-- * replace 'normalize' in Infer with this
-- * put fresh variables - ideally 'f', 'g', 'h' for functions, 'a', 'b', 'c' for other types
module SimplifyType
  ( simplifyScheme
  )
where

import           Type
import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.List                      ( (\\)
                                                , foldl'
                                                )

simplifyScheme :: Scheme -> Scheme
simplifyScheme (Forall tvs qt@(preds :=> _)) =
  Forall (tvs \\ M.keys (unSubst sub)) $ sub `apply` qt
 where
  sub = foldl'
    (\acc tv -> substFunctionVar (S.fromList preds) tv `compose` acc)
    emptySubst
    tvs

substFunctionVar :: S.Set Pred -> TVar -> Subst
substFunctionVar preds tv
  | typeKind f == arrowKind && filterRelevant f preds == S.fromList [PFun f]
  = Subst $ M.singleton tv typeLinArrow
  | typeKind f == arrowKind && filterRelevant f preds == S.fromList
    [PFun f, PUn f]
  = Subst $ M.singleton tv typeUnArrow
  | otherwise
  = emptySubst
  where f = TVar tv

filterRelevant :: Type -> S.Set Pred -> S.Set Pred
filterRelevant t = S.foldl' go mempty
 where
  go :: S.Set Pred -> Pred -> S.Set Pred
  go acc p@(IsIn _ ts) | t `elem` ts = S.insert p acc
                       | otherwise   = acc

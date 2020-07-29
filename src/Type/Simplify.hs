module Type.Simplify
  ( simplifyScheme
  )
where

import qualified Data.Set                      as S
import qualified Data.Map                      as M
import           Data.List                      ( (\\)
                                                , foldl'
                                                )
import           Type.Type

{- | This function is the top-level scheme (aka polytype)
    simplifier which returns a normalized version of a polytype
-}
simplifyScheme :: Scheme -> Scheme
simplifyScheme (Forall tvs qt@(preds :=> _)) =
  Forall (tvs \\ M.keys (unSubst sub)) $ sub `apply` qt
 where
  sub = foldl'
    (\acc tv -> substFunctionVar (S.fromList preds) tv `compose` acc)
    emptySubst
    tvs

-- | Substitutes @f@ for 'typeUnArrow' when @'PFun' f@ and @'PUn' f@ are found
substFunctionVar :: S.Set Pred -> TVar -> Subst
substFunctionVar preds tv
  | typeKind f == arrowKind && filterRelevant f preds == S.fromList
    [PFun f, PUn f]
  = Subst $ M.singleton tv typeUnArrow
  | otherwise
  = emptySubst
  where f = TVar tv


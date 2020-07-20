{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Type.Class
  ( ClassEnv()
  , classes
  , instances
  , initialClassEnv
  , substPred
  , isIn
  , member
  , updateInstances
  , hasFieldForStruct
  -- , addStruct
  )
where


import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )
import qualified Data.List.NonEmpty            as NE
import           Control.Lens
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Maybe                     ( isJust )

import           Core.Syntax
import           Type.Type
import           Name
import           Type.Unify

-- Assumes that union of '_fdFrom' and '_fdTo' is all of the 'TVar's possible in 'ClassInfo'
data FunDep = FunDep { _fdFrom :: [TVar], _fdTo :: [TVar] }
makeLenses ''FunDep

instance Pretty FunDep where
  pretty fd = PP.hsep (fd ^. fdFrom <&> pretty) <+> "~>" <+> PP.hsep
    (fd ^. fdTo <&> pretty)

data ClassInfo = ClassInfo { _cTyVars :: [TVar], _cResult :: Pred, _cFunDeps :: Maybe FunDep }
makeLenses ''ClassInfo

-- the instances are not required to be well-kinded
-- specifically, we lift Un even to function levels which wouldn't make sense
-- however, the manual instances are verified to be fine TODO explain this better
--
-- we would need kind variables to do this properly :/
-- i.e.
-- ```
-- type Un = forall k. k -> Constraint
-- class Un (x :: k) where ...
-- ```
data ClassInstance = ClassInstance { _iTyVars :: [TVar], {- _iPre :: [Pred],-} _iResult :: Pred }
makeLenses ''ClassInstance


prettyPredName :: Pred -> PP.Doc ann
prettyPredName (IsIn n _) = pretty n

instance Pretty ClassInfo where
  pretty ci =
    "class"
      <+> (ci ^. cResult & prettyPredName)
      <+> PP.sep (ci ^. cTyVars <&> pretty)
      <+> fdPretty (ci ^. cFunDeps)
   where
    fdPretty = \case
      Nothing -> ""
      Just f  -> "|" <+> pretty f

instance Pretty ClassInstance where
  pretty ci =
    "instance"
      -- <+> PP.tupled (ci ^. iPre <&> pretty)
      -- <+> "=>"
      <+> PP.sep (ci ^. iTyVars <&> pretty)
      <+> (ci ^. iResult & pretty)

type ClassInfoEnv = M.Map Name ClassInfo
type ClassInstanceEnv = M.Map Name [ClassInstance]

defaultClasses :: ClassInfoEnv
defaultClasses = M.fromList
  [ ("Un"      , unClass)
  , ("Fun"     , funClass)
  , ("Geq"     , geqClass)
  , ("HasField", hasFieldClass)
  ]
 where
  a             = TV "a" StarKind
  varA          = TVar a

  unClass       = ClassInfo [a] (PUn varA) Nothing

  f             = TV "f" arrowKind
  varF          = TVar f
  funClass      = ClassInfo [f] (PFun varF) Nothing

  b             = TV "b" StarKind
  varB          = TVar b
  geqClass      = ClassInfo [a, b] (PGeq varA varB) Nothing

  x             = TV "x" SymbolKind
  varX          = TVar x
  r             = TV "r" StarKind
  varR          = TVar r
  hasFieldClass = ClassInfo [x, r, a] (PHasField varX varR varA) (Just fDep)
  fDep          = FunDep [x, r] [a]

defaultInstances :: ClassInstanceEnv
defaultInstances = M.fromList
  [ ("Un"      , unInstances)
  , ("Fun"     , funInstances)
  , ("Geq"     , geqInstances)
  , ("HasField", hasFieldInstances)
  ]
 where
  unInstances =
    simpleInstance
      .   PUn
      <$> [typeChar, typeInt, typeBool, typeUnit, typeUnArrow]
  funInstances      = simpleInstance . PFun <$> [typeUnArrow, typeLinArrow]
  geqInstances      = []
  hasFieldInstances = []

simpleInstance :: Pred -> ClassInstance
simpleInstance result = ClassInstance [] {- [] -}result

data ClassEnv = ClassEnv { _classes :: !ClassInfoEnv, _instances :: !ClassInstanceEnv }
makeLenses ''ClassEnv

instance Pretty ClassEnv where
  pretty ce =
    PP.align
      $  PP.sep
      $  (ce ^. classes . to M.elems <&> pretty)
      <> (ce ^. instances . to M.elems <&> pretty)

instance Semigroup ClassEnv where
  e <> f =
    ClassEnv (e ^. classes <> f ^. classes) (e ^. instances <> f ^. instances)

instance Monoid ClassEnv where
  mempty = ClassEnv mempty mempty

initialClassEnv :: ClassEnv
initialClassEnv = ClassEnv defaultClasses defaultInstances

{-
structUnRule :: Struct -> ClassInstance -- no. check if fields which have a non-type-variable type are Un, then add the instance. sheesh.
structUnRule (Struct _ tcon _ fields _) = ClassInstance
  []
  (PUn . getType <$> fields)
  (PUn tcon)
 where
  -- TODO: Create lenses (folds) for types!
  getType :: StructField -> Type
  getType (StructField _ t _) = t
  -}

substPred :: ClassEnv -> Pred -> Maybe Subst
substPred ce p@(IsIn n ts)
  |
 -- -- --  | tracePretty (ce, p) False = undefined
    length ts /= length (cls ^. cTyVars)
  = error
    $  "caught invalid predicate!\n"
    <> show (pretty p)
    <> "\nof class:\n"
    <> show (pretty cls)
  | otherwise
  = foldMap (fitsInstance p) insts
 where
  cls   = ce ^?! classes . ix n
  insts = ce ^?! instances . ix n

  fitsInstance :: Pred -> ClassInstance -> Maybe Subst
  fitsInstance pr ci
    | pr == ci ^. iResult = Just emptySubst
    | otherwise = case unifyPreds (ci ^. iResult) pr of
      Left  _ -> Nothing
      Right s -> case cls ^. cFunDeps of
        Nothing -> Nothing
        Just fd ->
          let Subst nfSub = substToNormalForm pr
          in  let condition = M.keysSet nfSub == (S.fromList $ fd ^. fdFrom)
              in  if condition then Just s else Nothing

  substToNormalForm pr = case unifyPreds (cls ^. cResult) pr of
    Left err -> error $ "Wrong instance, couldn't unify!" <> show (pretty err)
    Right (Subst s) -> Subst $ M.filter (not . isTVar) s

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

isIn :: ClassEnv -> Pred -> Bool
isIn ce = isJust . substPred ce

member :: Pred -> ClassEnv -> Bool
member = flip isIn

unifyPreds :: Pred -> Pred -> Either UnificationError Subst
unifyPreds (IsIn a ts) (IsIn b us)
  | a /= b = error
    "not the same predicate, this should've been caught a long time ago!"
  | length ts /= length us = error
    "not the same length, this should've been caught a long time ago!"
  | otherwise = unifyMany (NE.toList ts) (NE.toList us)

updateInstances
  :: (Name, [ClassInstance]) -> ClassInstanceEnv -> ClassInstanceEnv
updateInstances (n, new) instances = instances & at n %~ fmap (new <>)

hasFieldForStruct :: Struct -> (Name, [ClassInstance])
hasFieldForStruct (Struct _ structType _ fields _) =
  ("HasField", makeInstance <$> fields)
 where
  makeInstance :: StructField -> ClassInstance
  makeInstance (StructField _ t n) =
    simpleInstance $ PHasField (TSym n) structType t

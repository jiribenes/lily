{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Class
  ( ClassEnv()
  , inEnv
  , singleton
  , fromList
  , initialClassEnv
  , addStruct
  )
where


import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Core.Syntax
import           Type.Type


newtype ClassEnv = ClassEnv { unClassEnv :: Pred -> Bool }

instance Semigroup ClassEnv where
  (ClassEnv p1) <> (ClassEnv p2) = ClassEnv $ \x -> p1 x || p2 x

instance Monoid ClassEnv where
  mempty = ClassEnv $ const False

inEnv :: Pred -> ClassEnv -> Bool
inEnv = flip unClassEnv

singleton :: Pred -> ClassEnv
singleton = ClassEnv . (==)

fromList :: [Pred] -> ClassEnv
fromList = ClassEnv . flip elem

initialClassEnv :: ClassEnv
initialClassEnv = fromList
  [ PNum typeInt -- TODO: Deprecate PNum

  , PUn typeChar
  , PUn typeInt
  , PUn typeBool
  , PUn typeUnArrow
  , PUn typeUnit

  , PFun typeArrow
  , PFun typeLinArrow
  , PFun typeUnArrow
  ]

-- TODO all these rules
data PredicateRule = PredicateRule { tyVars :: [TVar], preconditions :: [Pred], result :: Pred }

instance Pretty PredicateRule where
  pretty (PredicateRule{..}) = "forall" <+> pretty tyVars <+> "." <+> PP.tupled (pretty <$> preconditions) <+> "=>" <+> pretty result

simpleRule :: Pred -> PredicateRule
simpleRule result = PredicateRule [] [] result

structUnRule :: Struct -> PredicateRule
structUnRule (Struct _ tcon _ fields) = PredicateRule [] (PUn . getType <$> fields) (PUn tcon)
 where
  -- TODO: Create lenses (folds) for types!
  getType :: StructField -> Type
  getType (StructField _ t _) = t

matchRule :: (Pred -> Bool) -> PredicateRule -> Bool
matchRule env PredicateRule{..} = all env preconditions

addStruct :: ClassEnv -> Struct -> ClassEnv
addStruct env s = if doesSRuleMatch then env else env
  where
    sRule = structUnRule s
    doesSRuleMatch = matchRule (unClassEnv env) sRule
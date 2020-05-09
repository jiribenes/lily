{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Type.Constraint where

import           Control.Lens
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Set                      as S
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Core.Located
import           Core.Syntax                    ( Expr )
import           Name
import           Type.Type

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
  pretty (Simplified r) = "from simplification of:" <+> PP.indent 4 (pretty r)
  pretty (Generalized r) = "from generalization of:" <+> PP.indent 4 (pretty r)
  pretty (Instantiated r) = "from instantiation of:" <+> PP.indent 4 (pretty r)
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
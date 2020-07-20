{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lint
  ( lintProgram
  , Suggestion(..)
  )
where

import qualified Data.List.NonEmpty            as NE
import           Control.Lens
import qualified Language.C.Clang.Cursor       as C
import qualified Language.C.Clang as C
import           Data.Text                      ( Text )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import           Control.Monad.Except
import qualified Data.Text.Prettyprint.Doc     as PP
import           Data.Text.Prettyprint.Doc      ( (<+>)
                                                , Pretty(..)
                                                )

import           Core.Syntax
import           Core.Located
import           Type.Infer
import           Type.Type
import qualified Type.Unify                    as U
import qualified Clang.Type                    as C

import           Debug.Trace.Pretty -- TODO: remove this!
import Debug.Trace (traceShow)


data Suggestion = SuggestionInternalError Text C.Cursor
                | SuggestionUnificationError U.UnificationError Scheme Type C.Cursor
                | SuggestionUnifies Subst [Pred] C.Cursor

instance Pretty Suggestion where
  pretty = \case
    SuggestionInternalError t c -> PP.align
      (PP.sep
        [ "Encountered an internal error!"
        , pretty t
        , "at location: " <+> prettyLocation (loc c)
        ]
      )
    SuggestionUnificationError u sch t c -> PP.align
      (PP.sep
        [ "Encountered a unification error!"
        , pretty u
        , "when attempting to unify (inferred):"
        , pretty sch
        , "with (from Clang):"
        , pretty t
        , "at location: " <+> prettyLocation (loc c)
        ]
      )
    SuggestionUnifies sub preds c -> PP.align
       (PP.sep
        [ "Yay! We have a valid unification!"
        , pretty sub
        , "Next: Are the predicates correct?"
        , pretty preds
        , "at location: " <+> prettyLocation (loc c)
        ]
      )

lintProgram :: InferState -> Program -> [Suggestion]
lintProgram is toplevels = toplevels >>= lintTopLevel is

lintTopLevel :: InferState -> TopLevel -> [Suggestion]
lintTopLevel _  (TLStruct       _ ) = []
lintTopLevel is (TLLet          l ) = lintLet is l
lintTopLevel is (TLLetRecursive ls) = (NE.toList ls) >>= lintLet is

lintLet :: InferState -> Let -> [Suggestion]
lintLet is (Let c _ n _)
  | isNothing inferredType
  = [SuggestionInternalError ("Expected inferred type but haven't found any") c]
  | isNothing clangType
  = [SuggestionInternalError ("Expected a valid type given from Clang") c]
  | otherwise
  = examine c (fromJust inferredType) (fromJust clangType)
 where
  inferredType = is ^. typeEnv . at n
  clangType    = C.fromClangType =<< C.cursorType c

-- TODO lepsi nazev, dude
examine :: C.Cursor -> Scheme -> Type -> [Suggestion]
examine c inferredType clangType =
  case runExcept (inferredType `U.onewayUnifies` clangType) of
    Left  err -> traceShow (C.typeCanonicalType <$> C.cursorType c) $ [SuggestionUnificationError err inferredType clangType c]
    Right (sub, preds) -> [SuggestionUnifies sub preds c]

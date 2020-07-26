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
import qualified Language.C.Clang              as C
import           Data.Text                      ( Text )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                , catMaybes
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
import           Type.Constraint
import qualified Type.Unify                    as U
import qualified Type.Solve                    as S
import qualified Clang.Type                    as C

import           Debug.Trace.Pretty -- TODO: remove this!
import           Debug.Trace                    ( traceShow )


data Suggestion = SuggestionInternalError Text C.Cursor
                | SuggestionUnificationError U.UnificationError Scheme Type C.Cursor
                | SuggestionUnsatisfiedPredicate C.Cursor Pred
                | SuggestionUnrestrictedPointer C.Cursor Pred
                | SuggestionUnrestrictedRRef C.Cursor Pred
                | SuggestionUnrestrictedLinFunction C.Cursor Pred
                | SuggestionUnrestrictedSomething C.Cursor Pred Type

instance Pretty Suggestion where
  pretty = \case
    SuggestionInternalError t c -> PP.align
      (PP.vsep
        [ "Encountered an internal error!"
        , PP.indent 4 (pretty t)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        ]
      )
    SuggestionUnificationError u sch t c -> PP.align
      (PP.vsep
        [ "Encountered a unification error!"
        , PP.indent 4 (pretty u)
        , "when attempting to unify (inferred):"
        , PP.indent 4 (pretty sch)
        , "with (from Clang):"
        , PP.indent 4 (pretty t)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        ]
      )
    SuggestionUnsatisfiedPredicate c pred -> PP.align
      (PP.vsep
        [ "The following predicate could not be satisfied!"
        , PP.indent 4 (pretty pred)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        ]
      )
    SuggestionUnrestrictedPointer c pred -> PP.align
      (PP.vsep
        [ "The following predicate could not be satisfied!"
        , PP.indent 4 (pretty pred)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        , "This means that a pointer is either duplicated or discarded!"
        , ""
        ]
      )
    SuggestionUnrestrictedRRef c pred -> PP.align
      (PP.vsep
        [ "The following predicate could not be satisfied!"
        , PP.indent 4 (pretty pred)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        , "This means that a rvalue reference is either duplicated or discarded!"
        , ""
        ]
      )
    SuggestionUnrestrictedLinFunction c pred -> PP.align
      (PP.vsep
        [ "The following predicate could not be satisfied!"
        , PP.indent 4 (pretty pred)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        , "This means that a linear function is not used exactly once as it should be!"
        , ""
        ]
      )
    SuggestionUnrestrictedSomething c pred t -> PP.align
      (PP.vsep
        [ "The following predicate could not be satisfied!"
        , PP.indent 4 (pretty pred)
        , "at location:" <+> prettyLocation (loc c)
        , ""
        , "This means that a value of type '" <> pretty t <> "' is either duplicated or discarded!"
        , "Note that this might be a false positive -- this might be allowed for your type!"
        , ""
        ]
      )
lintProgram :: InferState -> Program -> [Suggestion]
lintProgram is toplevels = toplevels >>= lintTopLevel is

lintTopLevel :: InferState -> TopLevel -> [Suggestion]
lintTopLevel _  (TLStruct       _ ) = []
lintTopLevel is (TLLet          l ) = lintLet is l
lintTopLevel is (TLLetRecursive ls) = (NE.toList ls) >>= lintLet is

-- Note: This function does _NOT_ lint
-- constructors as we have no reliable way
-- of guessing that a function is actually a
-- constructor when calling it. 
--
-- This means that it needs to have both
-- type @() -> Struct@ and @()@ which is what
-- Clang thinks it should have. It's bonkers.
lintLet :: InferState -> Let -> [Suggestion]
lintLet is (Let _ LetConstructor _ _) = []
lintLet is (Let c LetFunction n _)
  | isNothing inferredType
  = [SuggestionInternalError ("Expected inferred type but haven't found any") c]
  | isNothing clangType
  = [SuggestionInternalError ("Expected a valid type given from Clang") c]
  | otherwise
  = suggest is c (fromJust inferredType) (fromJust clangType)
 where
  inferredType = is ^. typeEnv . at n
  clangType    = C.fromClangType =<< C.cursorType c

suggest :: InferState -> C.Cursor -> Scheme -> Type -> [Suggestion]
suggest is c inferredType clangType =
  case runExcept (inferredType `U.onewayUnifies` clangType) of
    Left err ->
      traceShow (C.typeCanonicalType <$> C.cursorType c)
        $ [SuggestionUnificationError err inferredType clangType c]
    Right (_, preds) ->
      let simplifiedPredicates =
              S.simplifyMany (is ^. classEnv) (S.fromPred JustBecause <$> preds)
      in  catMaybes $ makeUnsatSpecific c <$> S.toPreds simplifiedPredicates

makeUnsatSpecific :: C.Cursor -> Pred -> Maybe Suggestion
makeUnsatSpecific c p@(PUn (t `TAp` _)) | t == typePtr = pure $ SuggestionUnrestrictedPointer c p
                                        | t == typeRRef = pure $ SuggestionUnrestrictedRRef c p 
makeUnsatSpecific c p@(PUn t) | t == typeArrow = Nothing -- hotfix, sometimes a (Un ->) constraint appears from legacy code, this should not be presented to the user!
                              | t == typeLinArrow = pure $ SuggestionUnrestrictedLinFunction c p
                              | otherwise = pure $ SuggestionUnrestrictedSomething c p t
makeUnsatSpecific c p@(PFun t) | t == typeArrow = Nothing -- hotfix, sometimes a (Fun ->) constraint appears from legacy code, this should not be presented to the user!
makeUnsatSpecific c p = pure $ SuggestionUnsatisfiedPredicate c p

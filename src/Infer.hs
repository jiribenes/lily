{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Infer where

import qualified Assumption                    as A
import           Type
import           MonadFresh
import           Solve
import           Unify
import           Clang

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
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Foldable                  ( fold )
import qualified Data.Text as Text

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Language.C.Clang.Cursor ( cursorChildrenF, cursorChildren, cursorKind, cursorType, Cursor, CursorKind(..))
import           Language.C.Clang.Type ( typeCanonicalType, typeSpelling )
import qualified Language.C.Clang.Cursor.Typed as T

import           Debug.Trace -- TODO

newtype InferEnv = InferEnv { _typeEnv :: M.Map Name Scheme }
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''InferEnv

-- | Monomorphic variable set
type InferMonos = S.Set TVar

data InferError = FromSolve SolveError
               | UnboundVariables [Name]
               | WrongPredicate Pred
               | WrongConstraint [Constraint]
               | WrongConstructor Name
               | Weirdness
               -- | Ambiguous       [Constraint]
               -- | WrongKind       TVar Type

instance Show InferError where
  show (FromSolve x) = show x
  show (UnboundVariables as) =
    "Not in scope: " <> unlines [ "\t\t" <> show a | a <- as ]
  show (WrongPredicate   pred) = "Wrong predicate: " <> show pred
  show (WrongConstructor n   ) = "Wrong constructor: " <> show n
    -- show (Ambiguous cs) = unlines ["Cannot match expected type: " <> show a <> " with actual type: " <> show b  | (a, b) <- cs]
    -- show (WrongKind a b) = "Wrong kind!"

-- | Infer monad allows to: read monomorphic variables, create fresh variables and raise errors
type Infer a = ReaderT InferMonos (FreshT Text.Text (Except InferError)) a

runInfer :: Infer a -> Either InferError a
runInfer m =
  runExcept $ evalFreshT (runReaderT m S.empty) initialFreshState

-- | Infer a single type for an expression
inferExpr :: InferEnv -> FunctionCursor -> Either InferError Scheme
inferExpr env expr = case runInfer (inferType env expr) of
  Left err -> Left err
  Right (subst, preds, ty) ->
    Right $ closeOver (subst `apply` preds) (subst `apply` ty)

checkPred :: ClassEnv -> Pred -> Bool
checkPred classes pred = pred `S.member` classes

isConcrete (TCon _ ) = True
isConcrete (TAp x y) = isConcrete x && isConcrete y
isConcrete (TVar _ ) = False

isComplete (IsIn _ ts) = all isConcrete ts

-- | Take the accumulated constraints and run a solver over them
runSolve :: [Constraint] -> Infer (Subst, [Constraint])
runSolve cs = do
  freshSt <- getFresh
  case runSolveT freshSt (solve cs) of
    Left  err                -> throwError $ FromSolve err
    Right (result, newFresh) -> do
      setFresh newFresh
      pure result

inferType :: InferEnv -> FunctionCursor -> Infer (Subst, [Pred], Type)
inferType env expr = do
  (as, t, cs) <- infer (T.withoutKind expr)

  let unbounds = A.keysSet as `S.difference` (env ^. typeEnv . to M.keysSet)

  -- if we still have some free unknown variables which are 
  -- in `as` but not in `env` by this point, we need to report them as an error
  unless (S.null unbounds) $ throwError $ UnboundVariables (S.toList unbounds)

  -- pair known assumptions to environment types
  let cs' =
        [ CExpInst t s
        | (x, s) <- env ^. typeEnv . to M.toList
        , t      <- A.lookup x as
        ]

  -- solve all constraints together and get the resulting substitution
  (subst, unsolved) <- runSolve (cs <> cs')

  traceShowM
    ( "unsolved:"
    , unsolved
    , "subst:"
    , subst
    , "type:"
    , t
    , "subst-type"
    , subst `apply` t
    , "preds:"
    , unsolved
    , "subst-preds:"
    , subst `apply` unsolved
    )

    -- Ideally, do a difference between these lists, but that's too hard for now
  unless (length unsolved == length (toPreds unsolved))
    $ throwError
    $ WrongConstraint unsolved

  pure (subst, subst `apply` toPreds unsolved, subst `apply` t)

closeOver :: [Pred] -> Type -> Scheme
closeOver preds = normalize . generalize (fromPred <$> preds) S.empty

extendMonos :: TVar -> Infer a -> Infer a
extendMonos x = local (S.insert x)

extendManyMonos :: [TVar] -> Infer a -> Infer a
extendManyMonos xs = local (<> S.fromList xs)

-- helper constructors
makeFn2 :: Type -> Type -> Type
makeFn2 a b = typeArrow `TAp` a `TAp` b

makeFn3 :: Type -> Type -> Type -> Type
makeFn3 a b c = typeArrow `TAp` a `TAp` makeFn2 b c

makeArrow x f y = f `TAp` x `TAp` y

leq :: Type -> A.Assumption Type -> [Constraint]
leq t (A.Assumption as) = (`CGeq` t) . snd <$> as

wkn :: Name -> Type -> A.Assumption Type -> [Constraint]
wkn x t as | x `A.notMember` as = [CUn t]
           | otherwise          = []

unrestricted :: A.Assumption Type -> [Constraint]
unrestricted (A.Assumption as) = CUn . snd <$> as

-- Note: this can be made pointfree, but I don't think it's worth the effort...
freshFun :: Infer (Type, [Constraint])
freshFun = do
  f <- freshType arrowKind
  pure (f, [fromPred $ predFun f])

cand :: [Constraint] -> [Constraint]
cand xs = {-[CAnd-} xs{-]-}

-- | Take an expression and produce assumptions, result type and constraints to be solved
infer :: Cursor -> Infer (A.Assumption Type, Type, [Constraint])
infer cursor = case cursorKind cursor of
  IntegerLiteral -> pure (A.empty, typeInt, [])
  CXXBoolLiteralExpr -> pure (A.empty, typeBool, [])

  BinaryOperator -> do
    let [left, right] = cursorChildren cursor
    (as1, t1, cs1) <- infer left
    (as2, t2, cs2) <- infer right
    pure (as1 <> as2, t1, CEq t1 t2 : (cs1 <> cs2))
  
  -- parameter declaration
  ParmDecl -> do -- TODO: THIS IS NOT CORRECT
    pure (A.empty, typeInt, [])

  -- variable reference
  DeclRefExpr -> do -- TODO: THIS IS NOT CORRECT
      pure (A.empty, typeInt, [])

  FunctionDecl -> do
    let
      -- these are typically the first children but I really want to be safe here 
      parameterF :: Fold Cursor (T.CursorK 'ParmDecl)
      parameterF = cursorChildrenF . folding (T.matchKind @'ParmDecl)

      -- this is typically the last child but again, trying to be safe here!
      bodyF :: Fold Cursor (T.CursorK 'CompoundStmt)
      bodyF = cursorChildrenF . folding (T.matchKind @'CompoundStmt)

      parameters = cursor ^.. parameterF . to T.withoutKind
      [body] = cursor ^.. bodyF . to T.withoutKind

    (asList, tList, csList) <- unzip3 <$> traverse infer parameters
    (as2, t2, cs2) <- infer body

    -- THIS IS COMPLETELY INCORRECT! TODO TODO TODO
    pure (as2 <> fold asList, t2, cs2 <> fold csList)
  
  other -> do
    traceShowM $ "Found: '" <> show other <> "'. Will attempt to solve the situation as best as I can!"
    -- just gather the constraints and hope for the best.. :shrug:
    (asList, tList, csList) <- unzip3 <$> (traverse infer $ cursorChildren cursor)
    pure (fold asList, head tList, fold csList)

-- | This is the top-level function that should be used for inferring a type of something
inferTop :: InferEnv -> [(Name, FunctionCursor)] -> Either InferError InferEnv
inferTop env []                  = Right env
inferTop env ((name, expr) : xs) = case inferExpr env expr of
  Left  err -> Left err
  Right ty  -> inferTop (env & typeEnv . at name ?~ ty) xs

-- | Attempt to convert the type scheme into a normal(ish) format  
normalize :: Scheme -> Scheme
normalize (Forall origVars qt) = Forall
  (M.elems sub)
  ((properSub `apply` preds) :=> normtype body)
 where
  preds :=> body = qt
  bodyFV         = S.fromList $ fv body
  usefulVars     = S.toList $ S.fromList origVars `S.intersection` bodyFV
  sub            = M.fromList $ (\(old@(TV _ k), n) -> (old, TV n k)) <$> zip
    usefulVars
    (("t" <>) . Text.pack . show <$> [1 .. ])

  properSub = Subst $ M.map TVar sub

  fv :: Type -> [TVar]
  fv (TVar tv) = [tv]
  fv (TAp a b) = fv a ++ fv b
  fv (TCon _ ) = []

  normtype :: Type -> Type
  normtype (TAp a b)  = normtype a `TAp` normtype b
  normtype con@TCon{} = con
  normtype (TVar a)   = case a `M.lookup` sub of
    Just x  -> TVar x
    Nothing -> error "tv not in signature"

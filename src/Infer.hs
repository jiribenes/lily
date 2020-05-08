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
import           Clang
import           ClangType
import           Error

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
import           Data.Foldable                  ( foldrM
                                                , fold
                                                )
import qualified Data.Text                     as Text

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Language.C.Clang.Cursor        ( cursorSpelling
                                                , cursorChildrenF
                                                , cursorChildren
                                                , cursorKind
                                                , cursorType
                                                , Cursor
                                                , CursorKind(..)
                                                )
import qualified Language.C.Clang.Cursor.Typed as T

import           Debug.Trace -- TODO
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Language.C.Clang.Type          ( typeResultType )

newtype InferEnv = InferEnv { _typeEnv :: M.Map Name Scheme }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeLenses ''InferEnv

-- | Monomorphic variable set
type InferMonos = S.Set TVar

data InferError = FromSolve SolveError
                | UnboundVariables [Name]
                | WrongPredicate Pred
                | WrongConstraint [Constraint]
                | WrongConstructor Name
                | Weirdness -- TODO: elaborate on this further!
                | UnexpectedParameterDeclarationOutsideFunction
                | UnknownASTNodeKind CursorKind

instance Show InferError where
  show (FromSolve x) = show x
  show (UnboundVariables as) =
    "Not in scope: " <> unlines [ "\t\t" <> show a | a <- as ]
  show (WrongPredicate   pred) = "Wrong predicate: " <> show pred
  show (WrongConstructor n   ) = "Wrong constructor: " <> show n
  show (WrongConstraint  cs  ) = "Wrong constraints: " <> show cs
  show Weirdness               = "Encountered general weirdness! What?"
  show UnexpectedParameterDeclarationOutsideFunction
    = "Unexpected parameter declaration outside a function declaration. Is your AST ok?"
  show (UnknownASTNodeKind k) =
    "Unknown AST node: '" <> show k <> "'. Cannot recover!"
    -- show (Ambiguous cs) = unlines ["Cannot match expected type: " <> show a <> " with actual type: " <> show b  | (a, b) <- cs]
    -- show (WrongKind a b) = "Wrong kind!"

-- | Infer monad allows to: read monomorphic variables, create fresh variables and raise errors
newtype Infer a = Infer { unInfer :: ReaderT InferMonos (FreshT Name (Except InferError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader InferMonos, MonadFresh Name, MonadError InferError)

runInfer :: Infer a -> Either InferError a
runInfer m =
  runExcept $ evalFreshT (runReaderT (unInfer m) S.empty) initialFreshState

-- | Infer a single type for an expression
inferExpr :: InferEnv -> SomeFunctionCursor -> Either InferError Scheme
inferExpr env expr = case runInfer (inferType env expr) of
  Left err -> Left err
  Right (subst, preds, ty) ->
    Right $ closeOver (subst `apply` preds) (subst `apply` ty)

checkPred :: ClassEnv -> Pred -> Bool
checkPred classes pred = pred `S.member` classes

isConcrete :: Type -> Bool
isConcrete (TCon _ ) = True
isConcrete (TAp x y) = isConcrete x && isConcrete y
isConcrete (TVar _ ) = False

isComplete :: Pred -> Bool
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

inferType :: InferEnv -> SomeFunctionCursor -> Infer (Subst, [Pred], Type)
inferType env expr = do
  (as, t, cs) <- infer (unwrapSomeFunction expr)

  let unbounds = A.keysSet as `S.difference` (env ^. typeEnv . to M.keysSet)

  -- if we still have some free unknown variables which are 
  -- in `as` but not in `env` by this point, we need to report them as an error
  unless (S.null unbounds) $ throwError $ UnboundVariables (S.toList unbounds)

  -- pair known assumptions to environment types
  let cs' =
        [ CExpInst t s
        | (x, s) <- env ^. typeEnv . to M.toList
        , t      <- x `A.lookup` as
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

makeArrow :: Type -> Type -> Type -> Type
makeArrow x f y = f `TAp` x `TAp` y

leq :: Type -> A.Assumption Type -> [Constraint]
leq t (A.Assumption as) = (`CGeq` t) . snd <$> as

wkn :: Name -> Type -> A.Assumption Type -> [Constraint]
wkn x t as | x `A.notMember` as = [CUn t]
           | otherwise          = []

unrestricted :: A.Assumption Type -> [Constraint]
unrestricted (A.Assumption as) = CUn . snd <$> as

freshFun :: Infer (Type, [Constraint])
freshFun = do
  f <- freshType arrowKind
  pure (f, [fromPred $ PFun f])

-- | Take an expression and produce assumptions, result type and constraints to be solved
-- TODO: could this be nicer and more specific by using _singletons_ directly?
infer :: Cursor -> Infer (A.Assumption Type, Type, [Constraint])
infer cursor = case cursorKind cursor of
  IntegerLiteral     -> pure (A.empty, typeInt, [])
  CXXBoolLiteralExpr -> pure (A.empty, typeBool, [])
  CharacterLiteral   -> pure (A.empty, typeChar, [])

  BinaryOperator     -> do
    let [left, right] = cursorChildren cursor

    traceM $ show $ parseBinOp
      (fromJust $ T.matchKind @ 'BinaryOperator $ cursor)

    -- fromJust is justified here as `HasType 'BinaryOperator`
    let clangType       = fromJust $ cursorType cursor
    let operatorArgType = fromJust $ fromClangType clangType
    (as1, t1, cs1) <- infer left
    (as2, t2, cs2) <- infer right
    pure (as1 <> as2, t1, CEq t1 t2 : CEq t1 operatorArgType : (cs1 <> cs2))

  UnaryOperator -> do
    let [child] = cursorChildren cursor

    -- TODO: Handle the error more gracefully!
    unOp <- note Weirdness
      $ parseUnOp (fromJust $ T.matchKind @ 'UnaryOperator $ cursor)

    (as, t, cs) <- infer child

    case unOp of
      UnOpDeref -> do
        tv <- freshType StarKind
        pure (as, tv, CEq t (typePtrOf tv) : cs <> unrestricted as)
      UnOpAddrOf -> do
        tv <- freshType StarKind
        pure (as, tv, CEq tv (typePtrOf t) : cs <> unrestricted as)
      other -> do
        traceM $ "Best effort for unary operation: " <> show other <> "!"

        -- fromJust is justified here as `HasType 'UnaryOperator`
        let clangType = fromJust $ cursorType cursor >>= fromClangType
        pure (as, t, CEq t clangType : cs <> unrestricted as)

  -- parameter declaration
  ParmDecl    -> throwError UnexpectedParameterDeclarationOutsideFunction

  -- TODO: Variable declaration
  -- should become 
  --
  -- _let_ x = <RHS> _in_ <rest of the program>
  --
  -- How can we do this efficiently?
  -- Because we need to thread the values together!

  -- variable/function reference
  DeclRefExpr -> do
    tv <- freshType StarKind

    -- TODO: check if this is indeed the correct spelling!
    let name = decodeUtf8 $ cursorSpelling cursor
    pure
      ( A.singleton (Name name, tv)
      , tv  -- Note: We are actually never using the resulting type of this directly -- only indirectly through assumptions!
      , []
      )

  FunctionDecl     -> inferSomeFunction cursor
  FunctionTemplate -> inferSomeFunction cursor

  FirstExpr        -> inferSingleChild cursor
  CompoundStmt     -> inferLastChild cursor
  ReturnStmt       -> inferSingleChild cursor

  CallExpr         -> do
    -- here we're relying on the fact that the first child in AST tree will be a function reference
    -- and the rest are arguments
    let (functionRef : args) = cursorChildren cursor

    (fAs   , fT   , fCs   ) <- infer functionRef
    (asList, tList, csList) <- unzip3 <$> traverse infer args

    (fs, fsPreds)           <- unzip <$> traverse (const freshFun) args
    tv                      <- freshType StarKind


    let as    = fAs : asList
    let preds = nub $ unrestricted $ A.intersectMany as

    let functionType =
          foldr (\(t, f) acc -> makeArrow t f acc) tv $ zip tList fs

    pure
      ( fold as
      , tv
      , fCs <> fold csList <> preds <> fold fsPreds <> [CEq fT functionType]
      )

  other -> do
    traceM
      $  "Found: '"
      <> show other
      <> "'. Will attempt to solve the situation as best as I can!"
    let children = cursorChildren cursor

    traceM $ "This node has: " <> show (length children) <> " children!"

    -- if the cursor doesn't have any children, we're out of luck and give up
    when (null children) $ throwError $ UnknownASTNodeKind other

    if (length children == 1)
      then inferSingleChild cursor
      else inferLastChild cursor
      -- otherwise just gather the constraints and hope for the best.. :shrug:
      -- TODO: this should be hidden under some '--best-effort' kind of flag


inferSomeFunction :: Cursor -> Infer (A.Assumption Type, Type, [Constraint])
inferSomeFunction cursor = do
      -- these are typically the first children but I really want to be safe here 
  let parameterF :: Fold Cursor (T.CursorK 'ParmDecl)
      parameterF = cursorChildrenF . folding (T.matchKind @ 'ParmDecl)

    -- this is typically the last child but again, trying to be safe here!
  let bodyF :: Fold Cursor (T.CursorK 'CompoundStmt)
      bodyF = cursorChildrenF . folding (T.matchKind @ 'CompoundStmt)

  let parameters = cursor ^.. parameterF . to T.withoutKind
  let [body]     = cursor ^.. bodyF . to T.withoutKind

  -- TODO: extract this into its own function!
  (paramTypes, paramTVars) <-
    unzip <$> traverse (const $ freshTypeAndTVar StarKind) parameters
  let paramNames = cursor ^.. parameterF . to
        (Name . decodeUtf8 . cursorSpelling . T.withoutKind)
  -- TODO: Why don't ParamDecls have spelling? Investigate/fix!

  -- TODO: If we're a generic/template function, then some ParamDecls have a child "TypeRef"
  -- we could use to constrain the type even further!
  (as, returnType, cs) <- extendManyMonos paramTVars $ infer body

  (fs, fsPreds)        <- unzip <$> traverse (const freshFun) parameters -- arrow kind!
  let as'                   = as `A.removeMany` paramNames
  let wkns = (\(x, tv) -> wkn x tv as) =<< (zip paramNames paramTypes)
  let preds :: [Constraint] = nub ((fs >>= \f -> leq f as') <> wkns)

  let something             = zip paramTypes fs
  let functionType =
        foldr (\(tv, f) acc -> makeArrow tv f acc) returnType something

  -- TODO: our real return type isn't actually the inferred type of the body
  -- but rather a type variable that should unify with _all_ of the 'ReturnStmt'.
  -- This might be an important distinction but also it could be irrelevant seeing as Clang must verify the file first.
  let returnEqConstraint =
        maybe [] (\x -> [CEq x returnType])
          $   cursorType cursor
          >>= typeResultType
          >>= fromClangType

  pure
    ( as'
    , functionType
    , returnEqConstraint
    <> cs
    <> (eqConstraints paramNames paramTypes as)
    <> fold fsPreds
    <> preds
    )

-- | Call this for expressions that have a single child
-- and therefore are only some semantic wrappers!
inferSingleChild :: Cursor -> Infer (A.Assumption Type, Type, [Constraint])
inferSingleChild cursor = do
  let [child] = cursorChildren cursor
  (as, t, cs) <- infer child
  pure (as, t, cs)

inferLastChild :: Cursor -> Infer (A.Assumption Type, Type, [Constraint])
inferLastChild cursor = do
  (asList, tList, csList) <- unzip3 <$> traverse infer (cursorChildren cursor)
  pure (fold asList, last tList, fold csList)

-- Helper for infer @'FunctionDecl
-- TODO: Refactor!
eqConstraints :: [Name] -> [Type] -> A.Assumption Type -> [Constraint]
eqConstraints paramNames paramTypes as = do
  (x, tv) <- zip paramNames paramTypes
  t'      <- A.lookup x as
  pure $ CEq t' tv

-- | This is the top-level function that should be used for inferring a type of something
inferTop
  :: InferEnv -> [(Name, SomeFunctionCursor)] -> Either InferError InferEnv
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
    (Name . ("t" <>) . Text.pack . show <$> [1 ..])

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

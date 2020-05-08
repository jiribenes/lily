{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Clang
  ( isCanonical
  , createTranslationUnit
  , recursiveComponents
  , FunctionCursor
  , toFunction
  , FunctionTemplateCursor
  , toFunctionTemplate
  , SomeFunctionCursor(..)
  , someSpelling
  , someUSR
  , unwrapSomeFunction
  , printAST
  , BinOp(..)
  , parseBinOp
  , isAssignOp
  , UnaryFixity(..)
  , getFixity
  , UnOp(..)
  , parseUnOp
  )
where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BS
import           Data.Foldable                  ( traverse_ )
import           Data.Function                  ( on )
import qualified Data.Graph                    as G
import           Data.List                      ( groupBy
                                                , sortOn
                                                )
import           Language.C.Clang
import           Language.C.Clang.Cursor
import qualified Language.C.Clang.Cursor.Typed as T

import           Data.Maybe                     ( fromJust )

createTranslationUnit :: FilePath -> [String] -> IO TranslationUnit
createTranslationUnit filepath clangOptions = do
  idx <- createIndexWithOptions [DisplayDiagnostics]
  parseTranslationUnit idx filepath clangOptions

type FunctionCursor = T.CursorK 'FunctionDecl
type FunctionTemplateCursor = T.CursorK 'FunctionTemplate

data SomeFunctionCursor = SomeFunction FunctionCursor
                        | SomeFunctionTemplate FunctionTemplateCursor
    deriving stock (Eq, Show)

makePrisms ''SomeFunctionCursor

unwrapSomeFunction :: SomeFunctionCursor -> Cursor
unwrapSomeFunction (SomeFunction         f ) = T.withoutKind f
unwrapSomeFunction (SomeFunctionTemplate ft) = T.withoutKind ft

someUSR :: SomeFunctionCursor -> ByteString
someUSR = cursorUSR . unwrapSomeFunction

someSpelling :: SomeFunctionCursor -> ByteString
someSpelling = cursorSpelling . unwrapSomeFunction

-- | Returns true if this cursor is actually canonical!
isCanonical :: Cursor -> Bool
isCanonical c = cursorCanonical c == c

functionHasBody :: SomeFunctionCursor -> Bool
functionHasBody =
  any (\child -> cursorKind child == CompoundStmt)
    . cursorChildren
    . unwrapSomeFunction

toFunction :: Cursor -> Maybe FunctionCursor
toFunction = T.matchKind @ 'FunctionDecl

toFunctionTemplate :: Cursor -> Maybe FunctionTemplateCursor
toFunctionTemplate = T.matchKind @ 'FunctionTemplate

toSomeFunction :: Cursor -> Maybe SomeFunctionCursor
toSomeFunction c =
  (SomeFunction <$> toFunction c)
    <|> (SomeFunctionTemplate <$> toFunctionTemplate c)

calledFunctions :: Fold Cursor SomeFunctionCursor
calledFunctions = referencedCalls . functionDecls
 where
  referencedCalls :: Fold Cursor Cursor
  referencedCalls =
    cursorDescendantsF
      . folding (T.matchKind @ 'CallExpr)
      -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)
      . folding (fmap cursorCanonical . cursorReferenced . T.withoutKind)

  functionDecls :: Fold Cursor SomeFunctionCursor
  functionDecls = folding toSomeFunction

allFunctions :: Fold Cursor SomeFunctionCursor
allFunctions = cursorDescendantsF . folding toSomeFunction
   -- . filtered ({-isFromMainFile . -} rangeStart . T.cursorExtent)

getFunctionBody
  :: SomeFunctionCursor -> TranslationUnit -> Maybe SomeFunctionCursor
getFunctionBody def tu = translationUnitCursor tu ^? functionBodyF
 where
  functionBodyF :: Fold Cursor SomeFunctionCursor
  functionBodyF = allFunctions . filtered
    (\x ->
      (  cursorCanonical (unwrapSomeFunction x)
      == (unwrapSomeFunction def)
      && functionHasBody x
      )
    )

type FunctionGraphNode = (SomeFunctionCursor, ByteString, [ByteString])
type FunctionGraph = [FunctionGraphNode]

-- | TODO: We will always have at most one _real_ implementation, right?
normalize :: FunctionGraph -> FunctionGraph
normalize = fmap representGroup . groupByUSR
 where
  representGroup xs =
    ( fnCursor
    , cursorUSR $ unwrapSomeFunction fnCursor
    , xs ^.. traverse . _3 . traverse -- gather all `_3` in a single list
    )
   where
    fnCursor :: SomeFunctionCursor
    fnCursor = xs ^?! _head . _1 -- this is safe because when we get here in the actual program, 
                                       -- we already know that we have at least one cursor :)

  groupByUSR = groupBy ((==) `on` view _2) . sortOn (view _2)

recursiveComponents :: TranslationUnit -> [G.SCC SomeFunctionCursor]
recursiveComponents tu =
  translationUnitCursor tu
    ^.. allFunctions
    .   to intoGraphNode
    &   normalize
    &   G.stronglyConnComp
 where
  intoGraphNode :: SomeFunctionCursor -> FunctionGraphNode
  intoGraphNode fnDecl =
    ( fromJust $ getFunctionBody fnDecl tu
    , fnDecl & someUSR
    , (fnDecl & unwrapSomeFunction) ^.. calledFunctions . to someUSR
    )

printAST :: TranslationUnit -> IO ()
printAST tu = go 0 $ translationUnitCursor tu
 where
  go :: Int -> Cursor -> IO ()
  go i c = do
    let kind     = show $ cursorKind c
    let spelling = BS.unpack $ cursorSpelling c
    let canon    = if isCanonical c then " [canon]" else ""
    let hasBody =
          maybe "" (\x -> if functionHasBody x then "[has body]" else "")
            $ toSomeFunction c

    putStrLn $ indent i kind <> canon <> if null spelling
      then ""
      else ", " <> spelling <> if null hasBody then "" else ", " <> hasBody
    traverse_ (go (i + 4)) $ cursorChildren c

  indent :: Int -> String -> String
  indent i s = replicate i ' ' <> s

-- Should be synchronized to the 
-- [Clang source](https://github.com/llvm/llvm-project/blob/release/9.x/clang/include/clang/AST/OperationKinds.def)
data BinOp = BinOpPtrMemD
           | BinOpPtrMemI
           | BinOpMul
           | BinOpDiv
           | BinOpRem
           | BinOpAdd
           | BinOpSub
           | BinOpShl
           | BinOpShr
           | BinOpCmp
           | BinOpLT
           | BinOpGT
           | BinOpLE
           | BinOpGE
           | BinOpEQ
           | BinOpNE
           | BinOpAnd
           | BinOpXor
           | BinOpOr
           | BinOpLAnd
           | BinOpLOr
           | BinOpAssign
           | BinOpMulAssign
           | BinOpDivAssign
           | BinOpRemAssign
           | BinOpAddAssign
           | BinOpSubAssign
           | BinOpShlAssign
           | BinOpShrAssign
           | BinOpAndAssign
           | BinOpXorAssign
           | BinOpOrAssign
           | BinOpComma
  deriving stock (Eq, Ord, Show, Enum, Bounded)

isAssignOp :: BinOp -> Bool
isAssignOp o = o >= BinOpAssign && o <= BinOpOrAssign

parseBinOpToken :: ByteString -> Maybe BinOp
parseBinOpToken ".*"  = pure BinOpPtrMemD
parseBinOpToken "->*" = pure BinOpPtrMemI
parseBinOpToken "*"   = pure BinOpMul
parseBinOpToken "/"   = pure BinOpDiv
parseBinOpToken "%"   = pure BinOpRem
parseBinOpToken "+"   = pure BinOpAdd
parseBinOpToken "-"   = pure BinOpSub
parseBinOpToken "<<"  = pure BinOpShl
parseBinOpToken ">>"  = pure BinOpShr
parseBinOpToken "<=>" = pure BinOpCmp
parseBinOpToken "<"   = pure BinOpLT
parseBinOpToken ">"   = pure BinOpGT
parseBinOpToken "<="  = pure BinOpLE
parseBinOpToken ">="  = pure BinOpGE
parseBinOpToken "=="  = pure BinOpEQ
parseBinOpToken "!="  = pure BinOpNE
parseBinOpToken "&"   = pure BinOpAnd
parseBinOpToken "^"   = pure BinOpXor
parseBinOpToken "|"   = pure BinOpOr
parseBinOpToken "&&"  = pure BinOpLAnd
parseBinOpToken "||"  = pure BinOpLOr
-- assigns:
parseBinOpToken "="   = pure BinOpAssign
parseBinOpToken "*="  = pure BinOpMulAssign
parseBinOpToken "/="  = pure BinOpDivAssign
parseBinOpToken "%="  = pure BinOpRemAssign
parseBinOpToken "+="  = pure BinOpAddAssign
parseBinOpToken "-="  = pure BinOpSubAssign
parseBinOpToken "<<=" = pure BinOpShlAssign
parseBinOpToken ">>=" = pure BinOpShrAssign
parseBinOpToken "&="  = pure BinOpAndAssign
parseBinOpToken "^="  = pure BinOpXorAssign
parseBinOpToken "|="  = pure BinOpOrAssign
parseBinOpToken _     = Nothing


-- Should be synchronized to the 
-- [Clang source](https://github.com/llvm/llvm-project/blob/release/9.x/clang/include/clang/AST/OperationKinds.def)
data UnOp = UnOpPostInc
           | UnOpPostDec
           | UnOpPreInc
           | UnOpPreDec
           | UnOpAddrOf
           | UnOpDeref
           | UnOpPlus
           | UnOpMinus
           | UnOpNot
           | UnOpLNot
           | UnOpReal
           | UnOpImag
           | UnOpExtension
           | UnOpCoawait
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data UnaryFixity = Prefix | Postfix
  deriving stock (Eq, Ord, Show, Enum, Bounded)

getFixity :: UnOp -> UnaryFixity
getFixity op = if isPostfixOp op then Postfix else Prefix
  where isPostfixOp o = o `elem` [UnOpPreInc, UnOpPreDec]

parseUnOpToken :: UnaryFixity -> ByteString -> Maybe UnOp
parseUnOpToken Postfix "++"            = pure UnOpPostInc
parseUnOpToken Postfix "--"            = pure UnOpPostDec
parseUnOpToken Postfix _               = Nothing

parseUnOpToken Prefix  "++"            = pure UnOpPreInc
parseUnOpToken Prefix  "--"            = pure UnOpPreDec
parseUnOpToken Prefix  "&"             = pure UnOpAddrOf
parseUnOpToken Prefix  "*"             = pure UnOpDeref
parseUnOpToken Prefix  "+"             = pure UnOpPlus
parseUnOpToken Prefix  "-"             = pure UnOpMinus
parseUnOpToken Prefix  "~"             = pure UnOpNot
parseUnOpToken Prefix  "!"             = pure UnOpLNot
parseUnOpToken Prefix  "__real"        = pure UnOpReal
parseUnOpToken Prefix  "__imag"        = pure UnOpImag
parseUnOpToken Prefix  "__extension__" = pure UnOpExtension
parseUnOpToken Prefix  "co_await"      = pure UnOpCoawait
parseUnOpToken Prefix  _               = Nothing

locationEnd :: SourceRange -> Location
locationEnd = spellingLocation . rangeEnd

locationStart :: SourceRange -> Location
locationStart = spellingLocation . rangeStart

getLocationInterval :: SourceRange -> (Location, Location)
getLocationInterval = (,) <$> locationStart <*> locationEnd

-- | Checks if the first interval is a subinterval of the other one
-- i.e. is only true in the following case:
--
--  |-----|===========|--|
--  c     a           b  d
--
-- TODO: Can we use a library for this? How about [rampart](https://hackage.haskell.org/package/rampart-1.1.0.0/docs/Rampart.html)?
isSubintervalOf :: (Location, Location) -> (Location, Location) -> Bool
isSubintervalOf (a, b) (c, d)
  | file a /= file c = False
  | -- this should be impossible!
    offset a >= offset c && offset b <= offset d = True
  | -- TODO: fix this! It seems to be inclusive for UnOps but exclusive for BinOps.
    otherwise        = False
-- | This is super hacky and fragile! But libclang doesn't provide this functionality
-- and I don't want anyone trying to use this also building a fork of Clang.
-- That would be just silly...
--
-- TODO: test this!
parseBinOp :: T.CursorK 'BinaryOperator -> Maybe BinOp
parseBinOp cursor =
  case tokensUnderCursorInInterval cursor interestingInterval of
    [binOpToken] -> binOpToken & tokenSpelling & parseBinOpToken
    _            -> Nothing
 where
  [left, right]       = T.cursorChildren cursor
  leftExtent          = fromJust $ cursorExtent left
  rightExtent         = fromJust $ cursorExtent right
  interestingInterval = (locationEnd leftExtent, locationStart rightExtent)

parseUnOp :: T.CursorK 'UnaryOperator -> Maybe UnOp
parseUnOp cursor = case tokensUnderCursorInInterval cursor interval of
  [unOpToken] -> unOpToken & tokenSpelling & parseUnOpToken fixity
  _           -> Nothing
 where
  [child]            = T.cursorChildren cursor
  (fixity, interval) = getUnaryOpIntervalAndFixity cursor child

getUnaryOpIntervalAndFixity
  :: T.CursorK 'UnaryOperator -> Cursor -> (UnaryFixity, (Location, Location))
getUnaryOpIntervalAndFixity cursor child
  | startOffset cursorInterval == startOffset childInterval
  = (Postfix, (snd childInterval, snd cursorInterval))
  | endOffset cursorInterval == endOffset childInterval
  = (Prefix, (fst cursorInterval, fst childInterval))
  | otherwise
  = error "impossible"
 where
  cursorInterval = getLocationInterval . T.cursorExtent $ cursor
  childInterval  = getLocationInterval . fromJust . cursorExtent $ child

  startOffset    = offset . fst
  endOffset      = offset . snd

tokensUnderCursorInInterval
  :: T.HasExtent k => T.CursorK k -> (Location, Location) -> [Token]
tokensUnderCursorInInterval cursor int =
  cursor & allTokens & wrapInInterval & filterInInterval <&> fst
 where
  allTokens :: T.HasExtent k => T.CursorK k -> [Token]
  allTokens = tokenSetTokens . tokenize . T.cursorExtent

  wrapInInterval :: [Token] -> [(Token, (Location, Location))]
  wrapInInterval xs =
    zip xs $ xs <&> getLocationInterval . fromJust . tokenExtent

  filterInInterval
    :: [(Token, (Location, Location))] -> [(Token, (Location, Location))]
  filterInInterval = filter (\(_, tokInt) -> tokInt `isSubintervalOf` int)

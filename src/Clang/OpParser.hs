{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Clang.OpParser
  ( BinOp(..)
  , parseBinOp
  , isAssignOp
  , withoutAssign
  , UnaryFixity(..)
  , getFixity
  , UnOp(..)
  , parseUnOp
  )
where

import           Control.Lens
import           Data.ByteString.Char8          ( ByteString )
import           Data.Maybe                     ( fromJust )
import           Language.C.Clang.Cursor        ( Cursor
                                                , CursorKind(..)
                                                , cursorExtent
                                                )
import qualified Language.C.Clang.Cursor.Typed as T
import           Language.C.Clang.Location
import           Language.C.Clang.Token

-- | Represents all possible binary operations
--
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

-- | Checks if a 'BinOp' is an assign
isAssignOp :: BinOp -> Bool
isAssignOp o = o >= BinOpAssign && o <= BinOpOrAssign

-- | Returns a version of a 'BinOp' without an assign
withoutAssign :: BinOp -> Maybe BinOp
withoutAssign = \case
  BinOpMulAssign -> Just BinOpMul
  BinOpDivAssign -> Just BinOpDiv
  BinOpRemAssign -> Just BinOpRem
  BinOpAddAssign -> Just BinOpAdd
  BinOpSubAssign -> Just BinOpSub
  BinOpShlAssign -> Just BinOpShl
  BinOpShrAssign -> Just BinOpShr
  BinOpAndAssign -> Just BinOpAnd
  BinOpXorAssign -> Just BinOpXor
  BinOpOrAssign  -> Just BinOpOr
  _              -> Nothing

-- | Parses a 'BinOp' token
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

-- | Represents unary operators
--
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

-- | Fixity of a unary operation
data UnaryFixity = Prefix | Postfix
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Returns a fixity of a 'UnOp'
getFixity :: UnOp -> UnaryFixity
getFixity op = if isPostfixOp op then Postfix else Prefix
  where isPostfixOp o = o `elem` [UnOpPreInc, UnOpPreDec]

-- | Attempts to parse a 'UnOp'
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

-- | Returns the end 'Location' from a 'SourceRange' 
locationEnd :: SourceRange -> Location
locationEnd = spellingLocation . rangeEnd

-- | Returns the start 'Location' from a 'SourceRange' 
locationStart :: SourceRange -> Location
locationStart = spellingLocation . rangeStart

-- | Returns an interval of 'Locations' from a 'SourceRange'
getLocationInterval :: SourceRange -> (Location, Location)
getLocationInterval = (,) <$> locationStart <*> locationEnd

-- | Checks if the first interval is a subinterval of the other one
-- i.e. is only true in the following case:
--
-- @@
--  |-----|===========|--|
--  c     a           b  d
-- @@
isSubintervalOf :: (Location, Location) -> (Location, Location) -> Bool
isSubintervalOf (a, b) (c, d)
  | file a /= file c = False
  | offset a >= offset c && offset b <= offset d = True
  | otherwise        = False

-- | Parses a binary operator from its 'Cursor'
--
-- This is super hacky and fragile! But libclang doesn't provide this functionality
-- and I don't want anyone trying to use this also building a fork of Clang.
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

-- | Parses a unary operator from its 'Cursor'
--
-- This is super hacky and fragile! But libclang doesn't provide this functionality
-- and I don't want anyone trying to use this also building a fork of Clang.
parseUnOp :: T.CursorK 'UnaryOperator -> Maybe UnOp
parseUnOp cursor = case tokensUnderCursorInInterval cursor interval of
  [unOpToken] -> unOpToken & tokenSpelling & parseUnOpToken fixity
  _           -> Nothing
 where
  [child]            = T.cursorChildren cursor
  (fixity, interval) = getUnaryOpIntervalAndFixity cursor child

-- | Returns the fixity and interval of a unary operator
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

-- | Returns all tokens under a 'Cursor' which are in a specified interval
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


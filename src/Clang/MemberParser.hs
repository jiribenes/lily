{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Clang.MemberParser
  ( memberRHSSpelling
  )
where

import           Language.C.Clang.Cursor        ( CursorKind(..)
                                                , cursorSpelling
                                                )
import qualified Language.C.Clang.Cursor.Typed as T
import           Language.C.Clang.Token

import           Name

memberRHSSpelling :: T.CursorK 'MemberRefExpr -> Maybe Name
memberRHSSpelling c = if nameIsNull name
  then parseMemberSpelling c
  else Just name
 where
  s    = cursorSpelling $ T.withoutKind c
  name = nameFromBS s

allTokens :: T.HasExtent k => T.CursorK k -> [Token]
allTokens = tokenSetTokens . tokenize . T.cursorExtent

parseMemberSpelling :: T.CursorK 'MemberRefExpr -> Maybe Name
parseMemberSpelling c = case relevantTokens (allTokens c) of
  [x] -> Just $ nameFromBS x
  _   -> Nothing
 where
  relevantTokens = reverse . takeWhile (/= ".") . reverse . fmap tokenSpelling

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.GHCJS.Internal where


import           Data.JSString
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types

import           Control.Applicative
import           Data.Monoid
import           Unsafe.Coerce

import           Prelude                hiding (concat, null, drop)


data Page = Page JSString



renderRoute :: HashRoute -> JSString
renderRoute (HashRoute paths []) = intercalate "/" paths
renderRoute (HashRoute paths paramPairs) = concat [intercalate "/" paths
                                           , "?"
                                           , intercalate "&" params]
  where params = fmap (\(key,value) -> concat [key, "=", value]) paramPairs

data HashRoute = HashRoute {
  hashPaths  :: [JSString]
, hashParams :: [(JSString, JSString)]
}

emptyHashRoute :: HashRoute
emptyHashRoute = HashRoute [] []


parseHashRoute :: JSString -> HashRoute
parseHashRoute str = HashRoute (removeEmpty $ splitOn "/" pathStr) paramPairs
  where (pathStr, paramStr) = breakOn "?" str
        paramPairs = cleanPairs $ parseParams paramStr


cleanPairs :: [(JSString, JSString)] -> [(JSString, JSString)]
cleanPairs ((key, value):params) =
  if or [null key, null value]
    then params
    else (checkLeading key,drop 1 value):(cleanPairs params)
cleanPairs [] = []

checkLeading :: JSString -> JSString
checkLeading str = if index str 0 == '?'
                      then drop 1 str
                      else str

removeEmpty :: [JSString] -> [JSString]
removeEmpty (x:xs) =
  if null x
    then xs
    else x:(removeEmpty xs)
removeEmpty [] = []


parseParams :: JSString -> [(JSString, JSString)]
parseParams paramStr = makePairs <$> params
  where makePairs = breakOn "="
        params = splitOn "&" paramStr
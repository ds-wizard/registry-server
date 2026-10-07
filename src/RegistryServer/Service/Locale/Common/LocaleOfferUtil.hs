module RegistryServer.Service.Locale.Common.LocaleOfferUtil where

import qualified Data.List as L
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Text.Read (readMaybe)

import Shared.Util.String (splitOn)

selectOffered :: Maybe String -> (a -> String) -> (a -> String) -> [a] -> [a]
selectOffered mAppVersion getCode getVersion =
  fmap (selectForAppVersion (mAppVersion >>= parseVersion) getVersion . L.sortBy (comparing (versionKey . getVersion)))
    . L.groupBy (\a b -> getCode a == getCode b)
    . L.sortOn getCode

selectForAppVersion :: Maybe [Int] -> (a -> String) -> [a] -> a
selectForAppVersion mAppVersion getVersion sorted =
  case filter (isNotNewerThan mAppVersion . parseVersion . getVersion) sorted of
    [] -> head sorted
    older -> last older

isNotNewerThan :: Maybe [Int] -> Maybe [Int] -> Bool
isNotNewerThan _ Nothing = False
isNotNewerThan Nothing (Just _) = True
isNotNewerThan (Just appVersion) (Just version) = version <= appVersion

versionKey :: String -> (Bool, Maybe [Int])
versionKey version =
  let parsed = parseVersion version
   in (isNothing parsed, parsed)

parseVersion :: String -> Maybe [Int]
parseVersion version =
  case splitOn "." . reverse . takeWhile (/= ':') . reverse $ version of
    [] -> Nothing
    parts -> traverse readMaybe parts

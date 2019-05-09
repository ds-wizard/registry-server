module Util.String
  ( generateRandomString
  , lowerFirst
  , splitOn
  ) where

import Control.Monad (replicateM)
import Data.Char (toLower)
import qualified Data.Text as T
import System.Random (getStdRandom, randomR)

generateRandomString :: Int -> IO String
generateRandomString lengthOfString = replicateM lengthOfString (getStdRandom $ randomR ('a', 'z'))

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst [c] = [toLower c]
lowerFirst (s:str) = toLower s : str

splitOn :: String -> String -> [String]
splitOn separator string =
  case T.splitOn (T.pack separator) (T.pack string) of
    [""] -> []
    xs -> T.unpack <$> xs

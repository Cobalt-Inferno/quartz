{-# LANGUAGE OverloadedStrings #-}

module Build where

-- Scanning
import Data.Text (Text, splitOn)
import qualified Data.Text.IO as T
import Errors (Error(..), ErrorType(..), constructError)
import Text.StringConvert (toString)

-- Build
import System.Process (callCommand)

-- Scanning
data Result
  = Success
  | Failure Error
  deriving (Show, Eq)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' match (x:xs) = match == x || elem' match xs

scanWord :: Text -> Result
scanWord word =
  if elem' word bannedWords
    then Failure
           (constructError BannedWord $ "Word: '" <> word <> "' is not allowed.")
    else Success
  where
    bannedWords = ["curl", "wget"] -- We don't want people downloading more files, which could be potentially malicious.

isAll :: (Eq a) => a -> [a] -> Bool
isAll v (x:xs) = (v == x) && isAll v xs
isAll _ [] = False

takeFailure :: Result -> Error
takeFailure (Failure err) = err

strConcat :: [Text] -> Text
strConcat (x:xs) = x <> strConcat xs
strConcat [] = ""

concatWith :: [Text] -> Text -> Text
concatWith (x:xs) v = x <> v <> concatWith xs v
concatWith [] _ = ""

-- takeFailure Success = undefined
scanText :: Text -> Either Text [Error]
scanText txt =
  if isAll Success results
    then Left "Text is clean."
    else Right $ map takeFailure $ filter (/= Success) results
  where
    results = map scanWord $ splitOn "\n" $ strConcat $ splitOn " " txt

scanFile :: FilePath -> IO [Error]
scanFile file =
  T.readFile file >>= \contents ->
    case scanText contents of
      Left str -> return []
      Right err -> return err

-- Build
runCmd :: [Text] -> IO ()
runCmd args = callCommand . toString $ concatWith args " "

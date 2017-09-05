{-# LANGUAGE OverloadedStrings #-}

module Calculator (main) where

-- import System.Posix.Env.ByteString (getArgs)
import System.Environment (getArgs)
import Data.Yaml (decode)
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (readFile, putStrLn)

-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison

main :: IO ()
main = fileContent >>= \ x -> display x
  -- runMaybeT $ BS.putStrLn "horse"

display :: Maybe ByteString -> IO ()
display Nothing      = BS.putStrLn "Nothing found"
display (Just found) = BS.putStrLn found

fileContent :: IO (Maybe ByteString)
fileContent = (fmap . fmap) pack (decode <$> fileString)

fileString :: IO ByteString
fileString = filePath >>= BS.readFile

filePath :: IO String
filePath = flip (!!) 0 <$> getArgs

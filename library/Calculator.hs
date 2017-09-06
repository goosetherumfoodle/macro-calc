{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator (main) where

-- done:
-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison

-- todo:
-- 5. calc targets based on goals, body
-- 6. read from food library
-- 7. error handling
-- 8. refactor into smaller modules
-- 9. make suggestions from food library? (graph traversal?)

import System.Environment (getArgs)
import Data.Yaml (decode)
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (readFile, putStrLn, pack)
import Types

main :: IO ()
main = fileContent >>= display . fmap compareTargetAndTotals

display :: Show a => Maybe a -> IO ()
display Nothing      = BS.putStrLn "Nothing found"
display (Just found) = BS.putStrLn $ BS.pack $ show found

compareTargetAndTotals :: TargetAndDiet -> Macros
compareTargetAndTotals x = comparison (addDietTotals x) (target x) where
   -- todo: more specific types?
  comparison :: Macros -> Macros -> Macros
  comparison totals target = Macros (calories totals - calories target)
                                    (protein totals - protein target)
                                    (carbs totals - carbs target)
                                    (fat totals - fat target)

addDietTotals :: TargetAndDiet -> Macros
addDietTotals = runSumMacros . foldMap (SumMacros . calcMacros) . diet

-- todo: refactor
calcMacros :: Foodstuff -> Macros
calcMacros (Foodstuff (Macros cals prot carbs fat) (Quantity x) _) = Macros {
                                                                         calories = cals * x
                                                                       , protein  = prot * x
                                                                       , carbs    = carbs * x
                                                                       , fat      = fat * x
                                                                       }

fileContent :: IO (Maybe TargetAndDiet)
fileContent = decode <$> fileYAML

fileYAML :: IO ByteString
fileYAML = filePath >>= BS.readFile

filePath :: IO String
filePath = flip (!!) 0 <$> getArgs

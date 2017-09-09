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
-- 9. strategy for units?
-- 10. make suggestions from food library? (graph traversal?)

import System.Environment (getArgs)
import Data.Yaml (decodeEither', ParseException)
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (concat, readFile, putStrLn, pack)
import Types

main :: IO ()
main = dietPlan >>= display . fmap compareTargetAndTotals

display :: (Show a, Show b) => Either a b -> IO ()
display (Left error)  = BS.putStrLn $ BS.concat ["Shit! ", BS.pack $ show error]
display (Right found) = BS.putStrLn $ BS.pack $ show found

compareTargetAndTotals :: TargetAndDiet -> DiffMacros
compareTargetAndTotals = comparison <$> addDietTotals <*> target where
   -- todo: more specific types?
  comparison :: TotalMacros -> TargetMacros -> DiffMacros
  comparison (TotalMacros totals) (TargetMacros target) = DiffMacros $ Macros
                                                          (calories totals - calories target)
                                                          (protein totals - protein target)
                                                          (carbs totals - carbs target)
                                                          (fat totals - fat target)

addDietTotals :: TargetAndDiet -> TotalMacros
addDietTotals = runSumMacros . foldMap (SumMacros . TotalMacros . calcMacros) . diet

-- todo: refactor
calcMacros :: Foodstuff -> Macros
calcMacros (Foodstuff (FoodMacros (Macros cals prot carbs fat)) (Quantity x) _) = Macros {
                                                                         calories = cals * x
                                                                       , protein  = prot * x
                                                                       , carbs    = carbs * x
                                                                       , fat      = fat * x
                                                                       }

dietPlan :: IO (Either ParseException TargetAndDiet)
dietPlan = dietFilePath >>= fileContent

-- foodLibrary :: IO (Either ParseException TargetAndDiet)
-- foodLibrary = fileContent libraryFilePath

fileContent :: String -> IO (Either ParseException TargetAndDiet)
fileContent path = decodeEither' <$> fileYAML path

fileYAML :: String -> IO ByteString
fileYAML = BS.readFile

-- todo: use FilePath instead of String?
dietFilePath :: IO String
dietFilePath = flip (!!) 0 <$> getArgs

-- libraryFilePath :: String
-- libraryFilePath = "food-library.yml"

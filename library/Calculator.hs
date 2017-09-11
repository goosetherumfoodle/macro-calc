{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Calculator (main) where

-- done:
-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison
-- 6. read from food library

-- todo:
-- 6.5 refactor
-- 5. calc targets based on goals, body
-- 7. error handling
-- 8. refactor into smaller modules
-- 9. strategy for units?
-- 10. make suggestions from food library? (graph traversal?)

import System.Environment (getArgs)
import Data.Yaml (FromJSON, decodeEither')
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (concat, readFile, putStrLn, pack)
import qualified Data.Map as M
import Control.Monad (join)
import Types

-- move to 'nother namespace
maybeToEither :: a1 -> Maybe a -> Either a1 a
maybeToEither = flip maybe Right . Left

decodeEither'' :: forall b. (FromJSON b) => ByteString -> Either String b
decodeEither'' = stringifyException . decodeEither' where
  stringifyException :: (Show a) => Either a b -> Either String b
  stringifyException (Left e) = Left $ show e
  stringifyException (Right a) = Right a

main :: IO ()
main = do
  diet <- dietPlan
  lib <- foodLibrary
  food <- return $ join (fillInMacros <$> lib <*> diet)
  display $ compareTargetAndTotals <$> food

fillInMacros :: FoodLibrary -> TargetAndDescriptions -> Either String TargetAndFoods
fillInMacros foodLib tad = TargetAndFoods <$> (Right $ partialTarget tad) <*> (traverse (insertMacros foodLib) $ foodDescriptions tad) where
  insertMacros :: FoodLibrary -> FoodDescription -> Either String Food
  insertMacros lib fd = maybeToEither errorMsg $ Food <$> foundMacros <*> Just fd where
    errorMsg = Prelude.concat ["couldn't find ", (runFoodName $ foodName fd), " in the food library"]
    foundMacros = FoodMacros <$> (M.lookup (foodName fd) $ runFoodLibrary lib)

display :: (Show a, Show b) => Either a b -> IO ()
display (Left error)  = BS.putStrLn $ BS.concat ["Shit! ", BS.pack $ show error]
display (Right found) = BS.putStrLn $ BS.pack $ show found

compareTargetAndTotals :: TargetAndFoods -> DiffMacros
compareTargetAndTotals = comparison <$> addDietTotals <*> target where
   -- todo: more specific types?
  comparison :: TotalMacros -> TargetMacros -> DiffMacros
  comparison (TotalMacros totals) (TargetMacros target) = DiffMacros $ Macros
                                                          (calories totals - calories target)
                                                          (protein totals - protein target)
                                                          (carbs totals - carbs target)
                                                          (fat totals - fat target)

addDietTotals :: TargetAndFoods -> TotalMacros
addDietTotals = TotalMacros . runSumMacros . foldMap (SumMacros . calcMacros) . foods

-- todo: refactor
calcMacros :: Food -> Macros
calcMacros (Food (FoodMacros (Macros cals prot carbs fat))
                 (FoodDescription (Quantity x) _)) = Macros {
                                                         calories = cals * x
                                                       , protein  = prot * x
                                                       , carbs    = carbs * x
                                                       , fat      = fat * x
                                                       }

dietPlan :: IO (Either String TargetAndDescriptions)
dietPlan = dietFilePath >>= fmap decodeEither'' . fileYAML

foodLibrary :: IO (Either String FoodLibrary)
foodLibrary = (fmap . fmap) libratize foodList where
  foodList :: IO (Either String [FoodLibEntry])
  foodList = decodeEither'' <$> fileYAML libraryFilePath
  libratize :: [FoodLibEntry] -> FoodLibrary
  libratize = FoodLibrary . Prelude.foldr go M.empty
  go :: FoodLibEntry -> M.Map FoodName Macros -> M.Map FoodName Macros
  go (FoodLibEntry name (FoodMacros macros)) = M.insert name macros

fileYAML :: String -> IO ByteString
fileYAML = BS.readFile

-- todo: use FilePath instead of String?
dietFilePath :: IO String
dietFilePath = flip (!!) 0 <$> getArgs

libraryFilePath :: String
libraryFilePath = "food-library.yml"

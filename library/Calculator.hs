{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Calculator (main) where

-- done:
-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison
-- 5. read from food library
-- 6. error handling

-- todo:
-- 7. calc targets based on goals, body
-- 8. refactor into smaller modules
-- 9. strategy for units?
-- 10. make suggestions from food library? (graph traversal?)

import System.Environment (getArgs)
import Data.Yaml (ParseException, FromJSON, decodeEither')
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Control.Monad (join)
import Control.Applicative (liftA2)
import Control.Exception (throwIO)
import System.Exit (exitFailure)
import System.IO (stderr)
import Types

-- move to 'nother namespace
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

main :: IO ()
main = let food = join $ liftA2 fillInMacros foodLibrary dietPlan in
           compareTargetAndTotals <$> food >>= display

display :: Show a => a -> IO ()
display = BS.putStrLn . BS.pack . show

-- todo: refactor
fillInMacros :: FoodLibrary -> TargetAndDescriptions -> IO TargetAndFoods
fillInMacros foodLib tad = TargetAndFoods (partialTarget tad) <$> sequence ((insertMacros foodLib) <$> foodDescriptions tad) where

  insertMacros :: FoodLibrary -> FoodDescription -> IO Food
  insertMacros lib fd = Food <$> foundMacros <*> pure fd where

    foundMacros :: IO FoodMacros
    foundMacros = either notFoundFailure (return . FoodMacros) tryLookup

    tryLookup :: Either FoodName Macros
    tryLookup = maybeToEither (foodName fd) $ M.lookup (foodName fd) (runFoodLibrary lib)

    notFoundFailure :: FoodName -> IO FoodMacros
    notFoundFailure name = throwIO (FoodNotInLibrary name) >> exitFailure

compareTargetAndTotals :: TargetAndFoods -> DiffMacros
compareTargetAndTotals = comparison <$> addDietTotals <*> target where
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

decodeEither'' :: FromJSON a => FilePath -> BS.ByteString -> IO a
decodeEither'' path s = handleException (decodeEither' s) where
  handleException :: FromJSON a => Either ParseException a -> IO a
  handleException (Left e)  = throwIO $ ParseExceptionInFile e path
  handleException (Right a) = return a

dietPlan :: IO TargetAndDescriptions
dietPlan = dietFilePath >>= liftA2 (>>=) fileYAML decodeEither''

foodLibrary :: IO FoodLibrary
foodLibrary = libratize <$> parse  where
  parse :: IO [FoodLibEntry]
  parse = fileYAML libraryFilePath >>= decodeEither'' libraryFilePath

libratize :: [FoodLibEntry] -> FoodLibrary
libratize = FoodLibrary . Prelude.foldr go M.empty where
  go :: FoodLibEntry -> M.Map FoodName Macros -> M.Map FoodName Macros
  go (FoodLibEntry name (FoodMacros macros)) = M.insert name macros

fileYAML :: String -> IO BS.ByteString
fileYAML = BS.readFile

dietFilePath :: IO String
dietFilePath = getArgs >>= firstArg where
  firstArg :: [a] -> IO a
  firstArg (path:_) = return path
  firstArg _        = do BS.hPutStrLn stderr "Missing required argument: path to diet file"
                         exitFailure
libraryFilePath :: String
libraryFilePath = "food-library.yml"

{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics -- todo: limit?
import System.Environment (getArgs)
import Data.Yaml ((.:), withObject, FromJSON(parseJSON), ToJSON, decode)
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (readFile, putStrLn, pack)
import Data.ByteString.Char8

data TargetAndDiet = TargetAndDiet {
    target :: Macros
  , diet :: [Foodstuff]
  } deriving (Show, Generic)

instance ToJSON TargetAndDiet
instance FromJSON TargetAndDiet where
  parseJSON = withObject "targetAndDiet" $ \o -> do
    target        <- o .: "target"
    targetProtein <- target .: "protein"
    targetCarbs   <- target .: "carbs"
    targetFat     <- target .: "fat"
    targetCals    <- target .: "calories"
    diet          <- fmap parseJSON $ o .: "diet"
    TargetAndDiet (Macros {
                        calories = targetCals
                      , protein = targetProtein
                      , carbs = targetCarbs
                      , fat = targetFat
                      })
                   <$> diet

data Foodstuff = Foodstuff Macros Quantity FoodName deriving (Show, Generic)

newtype Quantity = Quantity Int deriving (Show, Generic)

newtype FoodName = FoodName String deriving (Show, Generic)

instance ToJSON FoodName
instance FromJSON FoodName where

instance ToJSON Foodstuff
instance FromJSON Foodstuff where
  parseJSON = withObject "foodstuff" $ \o -> do
    protein  <- o .: "protein"
    carbs    <- o .: "carbs"
    fat      <- o .: "fat"
    cals     <- o .: "calories"
    name     <- o .: "name"
    quantity <- o .: "quantity"
    return $ Foodstuff (Macros {
                                 calories = cals
                               , protein = protein
                               , carbs = carbs
                               , fat = fat
                               })
                        (Quantity quantity)
                        (FoodName name)

data Macros = Macros {
    calories :: Int
  , protein :: Int
  , carbs :: Int
  , fat :: Int
  } deriving (Show, Generic)

instance ToJSON Quantity
instance FromJSON Quantity

instance ToJSON Macros
instance FromJSON Macros

newtype SumMacros = SumMacros {runSumMacros :: Macros} deriving Show

instance Monoid SumMacros where
  mappend (SumMacros mac1) (SumMacros mac2) = SumMacros $ Macros {
                                                                calories = calories mac1 + calories mac2
                                                              , protein  = protein mac1 + protein mac2
                                                              , carbs    = carbs mac1 + carbs mac2
                                                              , fat      = fat mac1 + fat mac2
                                                              }
  mempty = SumMacros $ Macros {
                           calories = 0
                         , protein  = 0
                         , carbs    = 0
                         , fat      = 0
                         }

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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator (main) where

import GHC.Generics -- limit?
import System.Environment (getArgs)
import Data.Yaml ((.:), withObject, FromJSON(parseJSON), ToJSON, decode)
import Data.ByteString hiding (readFile, putStrLn)
import qualified Data.ByteString.Char8 as BS (readFile, putStrLn, pack)
import Data.ByteString.Char8

-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison

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

main :: IO ()
main = fileContent >>= display

display :: Show a => Maybe a -> IO ()
display Nothing      = BS.putStrLn "Nothing found"
display (Just found) = BS.putStrLn $ BS.pack $ show found

fileContent :: IO (Maybe TargetAndDiet)
fileContent = decode <$> fileString

fileString :: IO ByteString
fileString = filePath >>= BS.readFile

filePath :: IO String
filePath = flip (!!) 0 <$> getArgs

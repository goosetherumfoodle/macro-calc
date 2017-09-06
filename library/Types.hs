{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Yaml ((.:), withObject, FromJSON(parseJSON), ToJSON)

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

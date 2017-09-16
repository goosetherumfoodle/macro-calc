{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric
           , OverloadedStrings #-}

module Types where

import GHC.Generics (Generic)
import Data.Yaml ((.:), withObject, FromJSON(parseJSON), ToJSON)
import qualified Data.Map as M
import Control.Exception (Exception)
import Data.Yaml (ParseException)

data TargetAndFoods = TargetAndFoods {
    target :: TargetMacros
  , foods :: [Food]
  } deriving (Show, Generic)

data TargetAndDescriptions = TargetAndDescriptions {
    partialTarget :: TargetMacros
  , foodDescriptions :: [FoodDescription]
  } deriving Generic

data Food = Food {
    foodMacros :: FoodMacros
  , foodDescription :: FoodDescription
  } deriving (Show, Generic)

data FoodDescription = FoodDescription {
    foodQuantity :: Quantity
  , foodName :: FoodName
  } deriving (Show, Generic)

data Macros = Macros {
    calories :: Int
  , protein :: Int
  , carbs :: Int
  , fat :: Int
  } deriving (Show, Generic)

data ParseExceptionInFile = ParseExceptionInFile ParseException FilePath

data FoodNotInLibrary = FoodNotInLibrary FoodName

data FoodLibEntry = FoodLibEntry FoodName FoodMacros

newtype FoodLibrary = FoodLibrary {runFoodLibrary :: (M.Map FoodName Macros)}

newtype FoodMacros = FoodMacros {runFoodMacros :: Macros} deriving (Show, Generic)

newtype TotalMacros = TotalMacros Macros

newtype TargetMacros = TargetMacros Macros deriving (Show, Generic)

newtype DiffMacros = DiffMacros Macros deriving (Show)

newtype Quantity = Quantity Int deriving (Show, Generic)

newtype FoodName = FoodName {runFoodName :: String} deriving (Show, Generic, Eq, Ord)

newtype SumMacros = SumMacros {runSumMacros :: Macros}

instance Exception ParseExceptionInFile

instance Show ParseExceptionInFile where
  show (ParseExceptionInFile e path) = concat ["In file ", path, ": ", show e]

instance FromJSON FoodLibEntry where
  parseJSON = withObject "foodstuff" $ \o -> do
    protein  <- o .: "protein"
    carbs    <- o .: "carbs"
    fat      <- o .: "fat"
    cals     <- o .: "calories"
    name     <- o .: "name"
    return $ FoodLibEntry (FoodName name) (FoodMacros $ Macros {
                                              calories = cals
                                              , protein = protein
                                              , carbs = carbs
                                              , fat = fat
                                              })

instance FromJSON FoodDescription where
  parseJSON = withObject "FoodDescription" $ \o -> do
    name <- o .: "name"
    quantity <- o .: "quantity"
    return $ FoodDescription (Quantity quantity) (FoodName name)

instance FromJSON TargetAndDescriptions where
  parseJSON = withObject "targetAndDescriptions" $ \o -> do
    target        <- o .: "target"
    targetProtein <- target .: "protein"
    targetCarbs   <- target .: "carbs"
    targetFat     <- target .: "fat"
    targetCals    <- target .: "calories"
    diet          <- fmap parseJSON $ o .: "diet"
    TargetAndDescriptions (TargetMacros $ Macros {
                        calories = targetCals
                      , protein = targetProtein
                      , carbs = targetCarbs
                      , fat = targetFat
                      })
                   <$> diet

instance Exception FoodNotInLibrary

instance Show FoodNotInLibrary where
  show (FoodNotInLibrary a) = Prelude.concat [ "Shit! A food by the name of \""
                                             , runFoodName a
                                             , "\" was found in the diet plan, but not in the food library"
                                             ]

instance ToJSON TargetAndFoods

instance FromJSON TargetAndFoods where
  parseJSON = withObject "targetAndDiet" $ \o -> do
    target        <- o .: "target"
    targetProtein <- target .: "protein"
    targetCarbs   <- target .: "carbs"
    targetFat     <- target .: "fat"
    targetCals    <- target .: "calories"
    diet          <- fmap parseJSON $ o .: "diet"
    TargetAndFoods (TargetMacros $ Macros {
                        calories = targetCals
                      , protein = targetProtein
                      , carbs = targetCarbs
                      , fat = targetFat
                      })
                   <$> diet

instance ToJSON TargetMacros

instance ToJSON FoodMacros

instance ToJSON FoodName

instance FromJSON FoodName

instance ToJSON FoodDescription

instance ToJSON Food


instance FromJSON TargetMacros

instance ToJSON Quantity

instance FromJSON Quantity

instance ToJSON Macros

instance FromJSON Macros

instance FromJSON Food where -- todo: rename to FoodServing?
  parseJSON = withObject "foodstuff" $ \o -> do
    protein  <- o .: "protein"
    carbs    <- o .: "carbs"
    fat      <- o .: "fat"
    cals     <- o .: "calories"
    name     <- o .: "name"
    quantity <- o .: "quantity"
    return $ Food (FoodMacros $ Macros {
                                 calories = cals
                               , protein = protein
                               , carbs = carbs
                               , fat = fat
                               })
                              $ FoodDescription (Quantity quantity)
                                                (FoodName name)

instance Monoid SumMacros where
  mappend (SumMacros  mac1) (SumMacros  mac2) = SumMacros $ Macros {
                                                                calories = calories mac1 + calories mac2
                                                              , protein  = protein mac1 + protein mac2
                                                              , carbs    = carbs mac1 + carbs mac2
                                                              , fat      = fat mac1 + fat mac2
                                                              }
  mempty = SumMacros $  Macros {
                           calories = 0
                         , protein  = 0
                         , carbs    = 0
                         , fat      = 0
                         }

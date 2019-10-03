{-# LANGUAGE OverloadedStrings #-}
module Ex09.Common (
    Money
  , moneyDisplay
  , Product (..)
  , Stock (..)
  , carrot
  , celery
  , cucumber
  , Inputs(..)
  , MoneyInputs(..)
  , Error(..)
  , errorText
  , Ex09FnA
  , Ex09FnB
  ) where

import Data.Monoid ((<>))

import Data.Text
import qualified Data.Text as Text

import Reflex

type Money = Int

moneyDisplay ::
  Money ->
  Text
moneyDisplay =
  ("$" <>) . Text.pack . show

data Product =
  Product {
    pName :: Text
  , pCost :: Money
  } deriving (Eq, Ord, Show)

carrot ::
  Product
carrot =
  Product "Carrot" 1

celery ::
  Product
celery =
  Product "Celery" 2

cucumber ::
  Product
cucumber =
  Product "Cucumber" 3

data Stock =
  Stock {
    sProduct  :: Product
  , sQuantity :: Int
  } deriving (Eq, Ord, Show)

data Inputs t =
  Inputs {
    ibCarrot   :: Dynamic t Stock
  , ibCelery   :: Dynamic t Stock
  , ibCucumber :: Dynamic t Stock
  , ibSelected :: Dynamic t Text
  }

data Error =
    NotEnoughMoney
  | ItemOutOfStock
  deriving (Eq, Ord, Show)

data MoneyInputs t =
  MoneyInputs
    { mieSpend :: Event t Money
    , mieRefund :: Event t ()
    , mieAdd :: Event t ()
    }

errorText ::
  Error ->
  Text
errorText NotEnoughMoney =
  "Insufficient funds"
errorText ItemOutOfStock =
  "Item out of stock"

type Ex09FnA t m =
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)

type Ex09FnB t m =
  Inputs t ->
  m (Event t Text)

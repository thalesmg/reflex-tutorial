{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Ex05.Exercise where

import qualified Data.Map as Map

import Reflex
import Control.Monad (void)

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex05.Common
import Ex05.Run

ex05 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex05 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) =
  let
    bMoney = current dMoney
    bSelected = current dSelected
    bCarrot = current dCarrot
    bCelery = current dCelery
    bCucumber = current dCucumber
    tryBuy (money, pstock) = do
      void $ checkMoney money pstock
      checkStock pstock
    checkMoney money pstock =
      if money >= pCost (sProduct pstock)
      then Right pstock
      else Left NotEnoughMoney
    checkStock pstock =
      if sQuantity pstock > 0
      then Right $ sProduct pstock
      else Left ItemOutOfStock
    (eError, eBought)
      = fanEither
      . fmap tryBuy
      . attachWithMaybe
          (\maybeProduct money -> (money,) <$> maybeProduct)
          bmaybeProduct
      $ bMoney <@ eBuy
    bmaybeProduct = do
      productName <- bSelected
      case productName of
        "Carrot" -> Just <$> bCarrot
        "Celery" -> Just <$> bCelery
        "Cucumber" -> Just <$> bCucumber
        _ -> pure Nothing
    eVend =
      ffor eBought pName
    eSpend =
      ffor eBought pCost
    eChange =
      bMoney <@ eRefund
  in
    Outputs eVend eSpend eChange eError

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex05
#endif

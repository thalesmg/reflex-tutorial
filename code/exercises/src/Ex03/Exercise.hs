{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex03.Exercise where

import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex03.Common
import Ex03.Run

ex03 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex03 (Inputs bMoney bSelected eBuy eRefund) =
  let
    tryBuy money p =
      if pCost p <= money
      then Right p
      else Left p
    (eNotEnough, eBought)
      = fanEither
      . attachWith tryBuy bMoney
      $ eProduct
    eProduct =
      attachWithMaybe
        (\productName _ -> case productName of
            "Carrot" -> Just carrot
            "Celery" -> Just celery
            "Cucumber" -> Just cucumber
            _ -> Nothing)
        bSelected
        eBuy
    eVend =
      ffor eBought pName
    eSpend =
      ffor eBought pCost
    eChange =
      bMoney <@ eRefund
    eNotEnoughMoney =
      NotEnoughMoney <$ eNotEnough
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex03
#endif

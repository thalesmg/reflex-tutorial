{-# LANGUAGE CPP #-}
module Ex02.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex02.Common
import Ex02.Run

ex02 ::
  Reflex t =>
  Inputs t ->
  Outputs t
ex02 (Inputs bMoney eCarrot eCelery eCucumber eRefund) =
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
      leftmost [ carrot <$ eCarrot
               , celery <$ eCelery
               , cucumber <$ eCucumber
               ]
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
    host ex02
#endif

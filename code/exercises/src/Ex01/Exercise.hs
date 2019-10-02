{-# LANGUAGE CPP #-}
module Ex01.Exercise where

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex01.Common
import Ex01.Run

ex01 ::
  Reflex t =>
  Int ->
  Inputs t ->
  Outputs t
ex01 money (Inputs eCarrot eCelery eCucumber eRefund) =
  let
    tryBuy p =
      if pCost p <= money
      then Right p
      else Left p
    (eNotEnough, eBought)
      = fanEither
      . fmap tryBuy
      $ leftmost [ carrot <$ eCarrot
                 , celery <$ eCelery
                 , cucumber <$ eCucumber
                 ]
    eVend =
      ffor eBought pName
    eSpend =
      ffor eBought pCost
    eChange =
      const money <$> eRefund
    eNotEnoughMoney =
      () <$ eNotEnough
  in
    Outputs eVend eSpend eChange eNotEnoughMoney

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex01
#endif

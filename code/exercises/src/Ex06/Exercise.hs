{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Ex06.Exercise where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex06.Common
import Ex06.Run

dChange :: MonadHold t m
        => Reflex t
        => Event t ()
        -> Event t ()
        -> Dynamic t Money
        -> m (Dynamic t Money)
dChange eAny eRefund dMoney =
  holdDyn 0 $
    leftmost [ current dMoney <@ eRefund
             , 0 <$ eAny
             ]

dVend :: MonadHold t m
      => Reflex t
      => Event t Text
      -> Event t Error
      -> Event t ()
      -> m (Dynamic t Text)
dVend eVend eError eAny =
  holdDyn "" $
    leftmost [ errorText <$> eError
             , eVend
             , "" <$ eAny
             ]

ex06 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex06 (Inputs dMoney dCarrot dCelery dCucumber dSelected eBuy eRefund) = do
  let
    bMoney = current dMoney
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
          bmaybeStock
      $ bMoney <@ eBuy
    dmaybeStock = do
      productName <- dSelected
      case productName of
        "Carrot" -> Just <$> dCarrot
        "Celery" -> Just <$> dCelery
        "Cucumber" -> Just <$> dCucumber
        _ -> pure Nothing
    bmaybeStock = current dmaybeStock
    eVend =
      ffor eBought pName
    eSpend =
      ffor eBought pCost
    eChange =
      bMoney <@ eRefund
    eAny =
      leftmost [ () <$ updated dCarrot
               , () <$ updated dCelery
               , () <$ updated dCucumber
               , eBuy
               ]
  dChange' <- dChange eAny eRefund dMoney
  dVend' <- dVend eVend eError eAny
  pure (Outputs eVend eSpend eChange eError dChange' dVend')

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex06
#endif

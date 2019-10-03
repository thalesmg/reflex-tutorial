{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex07.Exercise where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex07.Common
import Ex07.Run

dynChange :: MonadHold t m
        => Reflex t
        => Event t ()
        -> Event t ()
        -> Dynamic t Money
        -> m (Dynamic t Money)
dynChange eAny eRefund dMoney =
  holdDyn 0 $
    leftmost [ current dMoney <@ eRefund
             , 0 <$ eAny
             ]

dynVend :: MonadHold t m
      => Reflex t
      => Event t Text
      -> Event t Error
      -> Event t ()
      -> m (Dynamic t Text)
dynVend eVend eError eAny =
  holdDyn "" $
    leftmost [ errorText <$> eError
             , eVend
             , "" <$ eAny
             ]

dynMoney :: MonadHold t m
       => MonadFix m
       => Reflex t
       => MoneyInputs t
       -> m (Dynamic t Money)
dynMoney (MoneyInputs eSpend eRefund eAdd) = mdo
  let
    isOverspend money price = money < price
    eOverspend =
      isOverspend <$> current dMoney <@> eSpend
    eSpendOk = difference eSpend (ffilter id eOverspend)
  dMoney <- foldDyn ($) 0 . mergeWith (.) $
    [ flip (-) <$> eSpendOk
    , const 0 <$ eRefund
    , (+ 1) <$ eAdd
    ]
  pure dMoney

ex07 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex07 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
  dMoney <- dynMoney (MoneyInputs eSpend eRefund eAdd)
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
  dChange <- dynChange eAny eRefund dMoney
  dVend <- dynVend eVend eError eAny
  pure (Outputs eVend eSpend eChange eError dMoney dChange dVend)

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex07
#endif

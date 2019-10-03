{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex09.Exercise where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex09.Common
import Ex09.Run

mkStock ::
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) =>
  Int ->
  Product ->
  Event t Text ->
  m (Dynamic t Stock)
mkStock initialQty prod eVended = mdo
  let
    isThisProduct pname = pname == pName prod
    hasStock pstock = sQuantity pstock > 0
    eVendedOk
      = ffilter hasStock
      $ current dStock
      <@ ffilter isThisProduct eVended
  dStock <- foldDyn ($) (Stock prod initialQty) $
              (\pstock -> pstock {sQuantity = sQuantity pstock - 1}) <$ eVendedOk
  pure dStock

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

dynVend :: MonadHold t m
        => Reflex t
        => Event t Product
        -> Event t Error
        -> m (Dynamic t Text)
dynVend eBought eError =
  holdDyn "" $
    leftmost [ pName <$> eBought
             , errorText <$> eError
             ]

ex09 :: MonadWidget t m
     => Inputs t ->
  m (Event t Text)
ex09 (Inputs dCarrot dCelery dCucumber dSelected) = mdo
  dMoney <- dynMoney (MoneyInputs eSpend eRefund eAdd)
  dChange <- dynChange eAny eRefund dMoney
  dVend <- dynVend eBought eError

  eBuy <- buyRow
  eAdd <- addMoneyRow dMoney
  eRefund <- refundRow dChange
  trayRow dVend

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
    eSpend =
      ffor eBought pCost
    eAny =
      leftmost [ () <$ updated dCarrot
               , () <$ updated dCelery
               , () <$ updated dCucumber
               , eBuy
               ]
    eVend = updated dVend

  pure eVend

buyRow :: MonadWidget t m
       => m (Event t ())
buyRow =
  el "tr" $ do
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      pure ()
    el "td" $
      button "Buy"

addMoneyRow :: MonadWidget t m
            => Dynamic t Money
            -> m (Event t ())
addMoneyRow dMoney =
  el "tr" $ do
    el "td" $
      text "Money inserted:"
    el "td" $
      pure ()
    el "td" $
      dynText (moneyDisplay <$> dMoney)
    el "td" $
      button "Add money"

refundRow :: MonadWidget t m
          => Dynamic t Money
          -> m (Event t ())
refundRow dChange =
  el "tr" $ do
    el "td" $
      text "Change:"
    el "td" $
      pure ()
    el "td" $
      dynText (moneyDisplay <$> dChange)
    el "td" $
      button "Refund"

trayRow :: MonadWidget t m
        => Dynamic t Text
        -> m ()
trayRow dVend =
  el "tr" $ do
    el "td" $
      text "Tray:"
    el "td" $
      pure ()
    el "td" $
      dynText dVend
    el "td" $
      pure ()

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host mkStock ex09
#endif

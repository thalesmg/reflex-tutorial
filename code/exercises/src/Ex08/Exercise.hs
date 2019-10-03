{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex08.Exercise where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Data.Text (Text)
import qualified Data.Map as Map

import Reflex

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex08.Common
import Ex08.Run

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

ex08 ::
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  ) =>
  Inputs t ->
  m (Outputs t)
ex08 (Inputs dCarrot dCelery dCucumber dSelected eAdd eBuy eRefund) = mdo
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
    host mkStock ex08
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Ex14.Exercise where

import Control.Lens ((^.))
import Control.Monad.Fix (MonadFix)

import Data.Text (Text, pack)
import qualified Data.Map as Map

import Reflex
import Reflex.Dom.Core

#ifndef ghcjs_HOST_OS
import Util.Run
#endif

import Ex14.Common
import Ex14.Run

radioButton ::
  MonadWidget t m
  => Eq a
  => Text
  -> Dynamic t a
  -> Dynamic t a
  -> m (Event t a)
radioButton groupName dValue dSelected = mdo
  ePostBuild <- getPostBuild
  let
    dBaseAttrs = Map.fromList [("type", "radio"), ("name", groupName)]
    dIsSelected = (==) <$> dValue <*> dSelected
    eValueChanged = leftmost [ True <$ eClicked
                             , current dIsSelected <@ ePostBuild
                             ]

    mkSelected True = Map.insert "checked" ""
    mkSelected False = Map.delete "checked"
  dAttr <- foldDyn ($) dBaseAttrs (mkSelected <$> eValueChanged)
  (e, _) <- elDynAttr' "input" dAttr (pure ())
  let eClicked = domEvent Click e
  pure $ current dValue <@ eValueChanged

radioCheckbox ::
  ( MonadWidget t m
  , Eq a
  ) =>
  Dynamic t a ->
  Dynamic t a ->
  m (Event t a)
radioCheckbox dValue dSelected = do
  eSelected <- radioButton "coisa" dValue dSelected
  pure (current dValue <@ eSelected)

stockWidget ::
  MonadWidget t m =>
  Dynamic t Stock ->
  Dynamic t Text ->
  m (Event t Text)
stockWidget dStock dSelected = mdo
  stockSnapshot <- sample (current dStock)
  let
    product = sProduct stockSnapshot
    dName = (pName . sProduct) <$> dStock
    r1 = text (pName product)
    r2 = dynText $ (pack . show . sQuantity) <$> dStock
    r3 = text $ moneyDisplay $ pCost product
    r4 = radioCheckbox dName dSelected

  row r1 r2 r3 r4

grid :: MonadWidget t m
     => m a
     -> m a
grid =
  elClass "div" "container"

row :: MonadWidget t m
    => m a
    -> m b
    -> m c
    -> m d
    -> m d
row ma mb mc md =
  elClass "div" "row" $ do
    elClass "div" "col-md-3" $
      ma
    elClass "div" "col-md-1" $
      mb
    elClass "div" "col-md-1" $
      mc
    elClass "div" "col-md-1" $
      md

buyRow :: MonadWidget t m
       => m (Event t ())
buyRow =
  let
    rBlank = pure ()
    r4 = button "Buy"
  in
    row rBlank rBlank rBlank r4

addMoneyRow :: MonadWidget t m
            => Dynamic t Money
            -> m (Event t ())
addMoneyRow dMoney =
  let
    rBlank = pure ()
    r1 = text "Money inserted:"
    r3 = dynText (moneyDisplay <$> dMoney)
    r4 = button "Add money"
  in
    row r1 rBlank r3 r4

refundRow :: MonadWidget t m
          => Dynamic t Money
          -> m (Event t ())
refundRow dChange =
  let
    rBlank = pure ()
    r1 = text "Change:"
    r3 = dynText (moneyDisplay <$> dChange)
    r4 = button "Refund"
  in
    row r1 rBlank r3 r4

trayRow :: MonadWidget t m
        => Dynamic t Text
        -> m ()
trayRow dVend =
  let
    rBlank = pure ()
    r1 = text "Tray:"
    r3 = dynText dVend
  in
    row r1 rBlank r3 rBlank

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

ex14 ::
  ( MonadWidget t m
  ) =>
  m ()
ex14 = mdo
  dCarrot <- mkStock 5 carrot eVended
  dCelery <- mkStock 5 celery eVended
  dCucumber <- mkStock 5 cucumber eVended

  dSelected <- holdDyn (pName carrot) $
    leftmost [ eCarrotSelected
             , eCelerySelected
             , eCucumberSelected
             ]

  Outputs eCarrotSelected eCelerySelected eCucumberSelected eVended <- grid $ mdo
    dMoney <- dynMoney (MoneyInputs eSpend eRefund eAdd)
    dChange <- dynChange eAny eRefund dMoney
    dVend <- dynVend eBought eError

    eCarrotSelected <- stockWidget dCarrot dSelected
    eCelerySelected <- stockWidget dCelery dSelected
    eCucumberSelected <- stockWidget dCucumber dSelected

    eBuy <- buyRow
    eAdd <- addMoneyRow dMoney
    eRefund <- refundRow dChange
    trayRow dVend

    let
      bMoney = current dMoney
      tryBuy (money, pstock) = do
        _ <- checkMoney money pstock
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

    pure $ Outputs eCarrotSelected eCelerySelected eCucumberSelected eVend
  pure ()

#ifndef ghcjs_HOST_OS
go ::
  IO ()
go =
  run $
    host ex14
#endif

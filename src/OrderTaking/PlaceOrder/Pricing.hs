-- | Pricing logic for the PlaceOrder workflow
-- Converted from F# PlaceOrder.Pricing.fs
module OrderTaking.PlaceOrder.Pricing
  ( -- * Pricing functions
    createPricingMethod
  , getPricingFunction
  ) where

import OrderTaking.Common.SimpleTypes
import OrderTaking.PlaceOrder.InternalTypes

-- | Create a pricing method given a promotionCode
createPricingMethod :: String -> PricingMethod
createPricingMethod promotionCode
  | null promotionCode || all (== ' ') promotionCode = Standard
  | otherwise = Promotion (PromotionCode promotionCode)

-- | Get pricing function based on pricing method
-- This is a stub implementation
getPricingFunction :: GetStandardPrices -> GetPromotionPrices -> GetPricingFunction
getPricingFunction standardPrices promoPrices = \pricingMethod ->
  case pricingMethod of
    Standard -> standardPrices ()
    Promotion promotionCode -> 
      let promoPrice = promoPrices promotionCode
          standardPrice = standardPrices ()
      in \productCode ->
        case promoPrice productCode of
          Just price -> price
          Nothing -> standardPrice productCode

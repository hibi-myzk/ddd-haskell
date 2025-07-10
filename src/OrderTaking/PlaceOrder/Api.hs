{-# LANGUAGE OverloadedStrings #-}

-- | This file contains the complete workflow, exposed as a JSON API
--
-- 1) The HttpRequest is turned into a DTO, which is then turned into a Domain object
-- 2) The main workflow function is called
-- 3) The output is turned into a DTO which is turned into a HttpResponse
--
-- Converted from F# PlaceOrder.Api.fs
module OrderTaking.PlaceOrder.Api
  ( -- * Types
    JsonString
  , HttpRequest(..)
  , HttpResponse(..)
  , PlaceOrderApi
    -- * API Implementation
  , placeOrderApi
  , workflowResultToHttpResponse
    -- * Dependencies (for testing/mocking)
  , checkProductExists
  , checkAddressExists
  , getStandardPrices
  , getPromotionPrices
  , getPricingFunction
  , calculateShippingCost
  , createOrderAcknowledgmentLetter
  , sendOrderAcknowledgment
  ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (encode, decode)
import OrderTaking.Common.SimpleTypes
import OrderTaking.PlaceOrder.InternalTypes
import OrderTaking.PlaceOrder.PublicTypes (PlaceOrderEvent, PlaceOrderError(..))
import OrderTaking.PlaceOrder.Dto (fromPlaceOrderEvent, fromPlaceOrderError, toUnvalidatedOrder)
import qualified OrderTaking.PlaceOrder.Implementation as Implementation
import qualified OrderTaking.PlaceOrder.Pricing as PricingModule

-- ======================================================
-- Types
-- ======================================================

-- | JSON string type alias
type JsonString = String

-- | Very simplified HTTP request
data HttpRequest = HttpRequest
  { action :: String
  , uri :: String
  , body :: JsonString
  } deriving (Show, Eq)

-- | Very simplified HTTP response
data HttpResponse = HttpResponse
  { httpStatusCode :: Int
  , responseBody :: JsonString
  } deriving (Show, Eq)

-- | An API takes an HttpRequest as input and returns an async response
type PlaceOrderApi = HttpRequest -> IO HttpResponse

-- ======================================================
-- Implementation
-- ======================================================

-- | Setup dummy dependencies

-- | Check if product exists (dummy implementation)
checkProductExists :: CheckProductCodeExists
checkProductExists _ = True -- dummy implementation

-- | Check if address exists (dummy implementation)
checkAddressExists :: CheckAddressExists
checkAddressExists unvalidatedAddress = do
  let checkedAddress = CheckedAddress unvalidatedAddress
  return $ Right checkedAddress

-- | Get standard prices
getStandardPrices :: GetStandardPrices
getStandardPrices () _ = unsafeCreatePrice 10.0

-- | Get promotion prices based on promotion code
getPromotionPrices :: GetPromotionPrices
getPromotionPrices (PromotionCode promotionCode) =
  let halfPricePromotion :: TryGetProductPrice
      halfPricePromotion productCode =
        if getProductCode productCode == "ONSALE"
        then Just $ unsafeCreatePrice 5.0
        else Nothing

      quarterPricePromotion :: TryGetProductPrice
      quarterPricePromotion productCode =
        if getProductCode productCode == "ONSALE"
        then Just $ unsafeCreatePrice 2.5
        else Nothing

      noPromotion :: TryGetProductPrice
      noPromotion _ = Nothing

  in case promotionCode of
       "HALF" -> halfPricePromotion
       "QUARTER" -> quarterPricePromotion
       _ -> noPromotion

-- | Get pricing function
getPricingFunction :: GetPricingFunction
getPricingFunction = PricingModule.getPricingFunction getStandardPrices getPromotionPrices

-- | Calculate shipping cost
calculateShippingCost :: CalculateShippingCost
calculateShippingCost = Implementation.calculateShippingCost

-- | Create order acknowledgment letter
createOrderAcknowledgmentLetter :: CreateOrderAcknowledgmentLetter
createOrderAcknowledgmentLetter _ = HtmlString "some text"

-- | Send order acknowledgment
sendOrderAcknowledgment :: SendOrderAcknowledgment
sendOrderAcknowledgment _ = Sent

-- ======================================================
-- Workflow
-- ======================================================

-- | Convert workflow result to HTTP response
workflowResultToHttpResponse :: Either PlaceOrderError [PlaceOrderEvent] -> HttpResponse
workflowResultToHttpResponse result = case result of
  Right events ->
    -- Turn domain events into DTOs
    let dtos = map fromPlaceOrderEvent events
        -- Serialize to JSON
        json = L8.unpack $ encode dtos
        response = HttpResponse
          { httpStatusCode = 200
          , responseBody = json
          }
    in response
  Left err ->
    -- Turn domain errors into a DTO
    let dto = fromPlaceOrderError err
        -- Serialize to JSON
        json = L8.unpack $ encode dto
        response = HttpResponse
          { httpStatusCode = 401
          , responseBody = json
          }
    in response

-- | Main place order API function
placeOrderApi :: PlaceOrderApi
placeOrderApi request = do
  -- Following the approach in "A Complete Serialization Pipeline" in chapter 11

  -- Start with a string
  let orderFormJson = body request

  -- Deserialize JSON to DTO
  case decode (L8.pack orderFormJson) of
    Nothing -> return $ HttpResponse
      { httpStatusCode = 400
      , responseBody = "{\"error\": \"Invalid JSON format\"}"
      }
    Just orderForm -> do
      -- Convert to domain object
      let unvalidatedOrder = toUnvalidatedOrder orderForm

      -- Setup the dependencies. See "Injecting Dependencies" in chapter 9
      let workflow = Implementation.placeOrder
            checkProductExists          -- dependency
            checkAddressExists          -- dependency
            getPricingFunction          -- dependency
            calculateShippingCost       -- dependency
            createOrderAcknowledgmentLetter  -- dependency
            sendOrderAcknowledgment     -- dependency

      -- Now we are in the pure domain
      asyncResult <- workflow unvalidatedOrder

      -- Now convert from the pure domain back to a HttpResponse
      return $ workflowResultToHttpResponse asyncResult

{-# LANGUAGE OverloadedStrings #-}

-- | Implementation of the PlaceOrder workflow
-- Converted from F# PlaceOrder.Implementation.fs
module OrderTaking.PlaceOrder.Implementation
  ( -- * Main workflow
    placeOrder
    -- * Individual step implementations
  , validateOrder
  , priceOrder
  , addShippingInfoToOrder
  , freeVipShipping
  , acknowledgeOrder
  , createEvents
    -- * Helper functions
  , toCustomerInfo
  , toAddress
  , toCheckedAddress
  , toOrderId
  , toOrderLineId
  , toProductCode
  , toOrderQuantity
  , toValidatedOrderLine
  , toPricedOrderLine
  , addCommentLine
  , getLinePrice
  , calculateShippingCost
  , makeShipmentLine
  , createShippingEvent
  , createBillingEvent
  , listOfOption
  ) where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Common.CompoundTypes
import OrderTaking.PlaceOrder.PublicTypes hiding (OrderAcknowledgmentSent(..))
import qualified OrderTaking.PlaceOrder.PublicTypes as Public
import OrderTaking.PlaceOrder.InternalTypes
import OrderTaking.PlaceOrder.Pricing (createPricingMethod)
import OrderTaking.Result hiding (sequence)
import qualified OrderTaking.Result as Result
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)

-- ======================================================
-- Section 2 : Implementation
-- ======================================================

-- ---------------------------
-- ValidateOrder step
-- ---------------------------

-- | Convert unvalidated customer info to validated customer info
toCustomerInfo :: UnvalidatedCustomerInfo -> Result ValidationError CustomerInfo
toCustomerInfo unvalidatedCustomerInfo' = do
  firstName' <- createString50 "FirstName" (unvalidatedFirstName unvalidatedCustomerInfo')
  lastName' <- createString50 "LastName" (unvalidatedLastName unvalidatedCustomerInfo')
  emailAddress' <- createEmailAddress "EmailAddress" (unvalidatedEmailAddress unvalidatedCustomerInfo')
  vipStatus' <- createVipStatus "VipStatus" (unvalidatedVipStatus unvalidatedCustomerInfo')

  let customerInfo = CustomerInfo
        { customerName = PersonalName { firstName = firstName', lastName = lastName' }
        , emailAddress = emailAddress'
        , vipStatus = vipStatus'
        }
  return customerInfo

-- | Convert checked address to validated address
toAddress :: CheckedAddress -> Result ValidationError Address
toAddress (CheckedAddress unvalidatedAddress) = do
  addressLine1' <- createString50 "AddressLine1" (unvalidatedAddressLine1 unvalidatedAddress)
  addressLine2' <- createString50Option "AddressLine2" (unvalidatedAddressLine2 unvalidatedAddress)
  addressLine3' <- createString50Option "AddressLine3" (unvalidatedAddressLine3 unvalidatedAddress)
  addressLine4' <- createString50Option "AddressLine4" (unvalidatedAddressLine4 unvalidatedAddress)
  city' <- createString50 "City" (unvalidatedCity unvalidatedAddress)
  zipCode' <- createZipCode "ZipCode" (unvalidatedZipCode unvalidatedAddress)
  state' <- createUsStateCode "State" (unvalidatedState unvalidatedAddress)
  country' <- createString50 "Country" (unvalidatedCountry unvalidatedAddress)

  let address = Address
        { addressLine1 = addressLine1'
        , addressLine2 = addressLine2'
        , addressLine3 = addressLine3'
        , addressLine4 = addressLine4'
        , city = city'
        , zipCode = zipCode'
        , state = state'
        , country = country'
        }
  return address

-- | Call the checkAddressExists and convert the error to a ValidationError
toCheckedAddress :: CheckAddressExists -> UnvalidatedAddress -> AsyncResult ValidationError CheckedAddress
toCheckedAddress checkAddress address = do
  result <- checkAddress address
  case result of
    Left AddressNotFound -> return $ Left $ ValidationError "Address not found"
    Left InvalidFormat -> return $ Left $ ValidationError "Address has bad format"
    Right checkedAddr -> return $ Right checkedAddr

-- | Convert string to OrderId
toOrderId :: String -> Result ValidationError OrderId
toOrderId = createOrderId "OrderId"

-- | Helper function for validateOrder - convert string to OrderLineId
toOrderLineId :: String -> Result ValidationError OrderLineId
toOrderLineId = createOrderLineId "OrderLineId"

-- | Helper function for validateOrder - convert and validate ProductCode
toProductCode :: CheckProductCodeExists -> String -> Result ValidationError ProductCode
toProductCode checkProductCodeExists productCode = do
  -- First create the ProductCode
  productCode' <- createProductCode "ProductCode" productCode
  -- Then check if it exists
  if checkProductCodeExists productCode'
    then Right productCode'
    else Left $ ValidationError $ "Invalid: " ++ show productCode'

-- | Helper function for validateOrder - convert quantity
toOrderQuantity :: ProductCode -> Double -> Result ValidationError OrderQuantity
toOrderQuantity = createOrderQuantity "OrderQuantity"

-- | Helper function for validateOrder - convert order line
toValidatedOrderLine :: CheckProductCodeExists -> UnvalidatedOrderLine -> Result ValidationError ValidatedOrderLine
toValidatedOrderLine checkProductExists unvalidatedOrderLine = do
  orderLineId' <- toOrderLineId (unvalidatedOrderLineId unvalidatedOrderLine)
  productCode' <- toProductCode checkProductExists (unvalidatedProductCode unvalidatedOrderLine)
  quantity' <- toOrderQuantity productCode' (unvalidatedQuantity unvalidatedOrderLine)

  let validatedOrderLine = ValidatedOrderLine
        { validatedOrderLineId = orderLineId'
        , validatedProductCode = productCode'
        , validatedQuantity = quantity'
        }
  return validatedOrderLine

-- | Main validation function
validateOrder :: ValidateOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = do
  runExceptT $ do
    -- Convert OrderId
    orderId <- ExceptT $ asyncResultOfResult $
      toOrderId (unvalidatedOrderId unvalidatedOrder)

    -- Convert CustomerInfo
    customerInfo <- ExceptT $ asyncResultOfResult $
      toCustomerInfo (unvalidatedCustomerInfo unvalidatedOrder)

    -- Check and convert shipping address
    checkedShippingAddress <- ExceptT $ toCheckedAddress checkAddressExists (unvalidatedShippingAddress unvalidatedOrder)
    shippingAddress' <- ExceptT $ asyncResultOfResult $ toAddress checkedShippingAddress

    -- Check and convert billing address
    checkedBillingAddress <- ExceptT $ toCheckedAddress checkAddressExists (unvalidatedBillingAddress unvalidatedOrder)
    billingAddress' <- ExceptT $ asyncResultOfResult $ toAddress checkedBillingAddress

    -- Convert order lines
    lines' <- ExceptT $ asyncResultOfResult $ Result.sequence $ map (toValidatedOrderLine checkProductCodeExists) (unvalidatedLines unvalidatedOrder)

    -- Create pricing method
    let pricingMethod = createPricingMethod (unvalidatedPromotionCode unvalidatedOrder)

    let validatedOrder = ValidatedOrder
          { validatedOrderId = orderId
          , validatedCustomerInfo = customerInfo
          , validatedShippingAddress = shippingAddress'
          , validatedBillingAddress = billingAddress'
          , validatedLines = lines'
          , validatedPricingMethod = pricingMethod
          }
    return validatedOrder

-- ---------------------------
-- PriceOrder step
-- ---------------------------

-- | Convert validated order line to priced order line
toPricedOrderLine :: GetProductPrice -> ValidatedOrderLine -> Result PricingError PricedOrderLine
toPricedOrderLine getProductPrice validatedOrderLine = do
  let qty = getOrderQuantityValue (validatedQuantity validatedOrderLine)
  let price = getProductPrice (validatedProductCode validatedOrderLine)
  linePrice' <- case multiplyPrice qty price of
    Left (ValidationError err) -> Left (PricingError err)
    Right lp -> Right lp

  let pricedLine = PricedOrderProductLine
        { pricedOrderLineId = validatedOrderLineId validatedOrderLine
        , pricedProductCode = validatedProductCode validatedOrderLine
        , pricedQuantity = validatedQuantity validatedOrderLine
        , pricedLinePrice = linePrice'
        }
  return (ProductLine pricedLine)

-- | Add comment line if needed based on pricing method
addCommentLine :: PricingMethod -> [PricedOrderLine] -> [PricedOrderLine]
addCommentLine Standard lines' = lines'
addCommentLine (Promotion (PromotionCode promoCode)) lines' =
  let commentLine = CommentLine $ "Applied promotion " ++ promoCode
  in lines' ++ [commentLine]

-- | Get the price from a priced order line
getLinePrice :: PricedOrderLine -> Price
getLinePrice (ProductLine line) = pricedLinePrice line
getLinePrice (CommentLine _) = unsafeCreatePrice 0.0

-- | Main pricing function
priceOrder :: PriceOrder
priceOrder getPricingFunction validatedOrder =
  let getProductPrice = getPricingFunction (validatedPricingMethod validatedOrder)
  in case Result.sequence $ map (toPricedOrderLine getProductPrice) (validatedLines validatedOrder) of
       Left err -> Left err
       Right lines' ->
         let linesWithComments = addCommentLine (validatedPricingMethod validatedOrder) lines'
             linePrices = map getLinePrice linesWithComments
         in case sumPrices linePrices of
              Left (ValidationError err) -> Left (PricingError err)
              Right amountToBill' ->
                let pricedOrder' = PricedOrder
                      { pricedOrderId = validatedOrderId validatedOrder
                      , pricedCustomerInfo = validatedCustomerInfo validatedOrder
                      , pricedShippingAddress = validatedShippingAddress validatedOrder
                      , pricedBillingAddress = validatedBillingAddress validatedOrder
                      , pricedAmountToBill = amountToBill'
                      , pricedLines = linesWithComments
                      , pricedPricingMethod = validatedPricingMethod validatedOrder
                      }
                in Right pricedOrder'

-- ---------------------------
-- Shipping step
-- ---------------------------

-- | Pattern match on address to determine shipping zone
data ShippingZone = UsLocalState | UsRemoteState | International

classifyAddress :: Address -> ShippingZone
classifyAddress address
  | getString50 (country address) == "US" =
      case getUsStateCode (state address) of
        "CA" -> UsLocalState
        "OR" -> UsLocalState
        "AZ" -> UsLocalState
        "NV" -> UsLocalState
        _ -> UsRemoteState
  | otherwise = International

-- | Calculate shipping cost based on address
calculateShippingCost :: CalculateShippingCost
calculateShippingCost pricedOrder' =
  case classifyAddress (pricedShippingAddress pricedOrder') of
    UsLocalState -> unsafeCreatePrice 5.0
    UsRemoteState -> unsafeCreatePrice 10.0
    International -> unsafeCreatePrice 20.0

-- | Add shipping info to order
addShippingInfoToOrder :: AddShippingInfoToOrder
addShippingInfoToOrder calculateShippingCost' pricedOrder' =
  let shippingInfo' = ShippingInfo
        { shippingMethod = Fedex24
        , shippingCost = calculateShippingCost' pricedOrder'
        }
  in PricedOrderWithShippingMethod
       { shippingInfo = shippingInfo'
       , pricedOrder = pricedOrder'
       }

-- ---------------------------
-- VIP shipping step
-- ---------------------------

-- | Update the shipping cost if customer is VIP
freeVipShipping :: FreeVipShipping
freeVipShipping order =
  let updatedShippingInfo = case vipStatus (pricedCustomerInfo (pricedOrder order)) of
        Normal -> shippingInfo order
        Vip -> (shippingInfo order)
          { shippingCost = unsafeCreatePrice 0.0
          , shippingMethod = Fedex24
          }
  in order { shippingInfo = updatedShippingInfo }

-- ---------------------------
-- AcknowledgeOrder step
-- ---------------------------

-- | Acknowledge order and create event if successful
acknowledgeOrder :: AcknowledgeOrder
acknowledgeOrder createAcknowledgmentLetter sendAcknowledgment pricedOrderWithShipping =
  let pricedOrder' = pricedOrder pricedOrderWithShipping
      letter = createAcknowledgmentLetter pricedOrderWithShipping
      acknowledgment = OrderAcknowledgment
        { ackEmailAddress = emailAddress (pricedCustomerInfo pricedOrder')
        , ackLetter = letter
        }
  in case sendAcknowledgment acknowledgment of
       Sent ->
         let event = Public.OrderAcknowledgmentSent
               { Public.ackOrderId = pricedOrderId pricedOrder'
               , Public.ackEmailAddress = emailAddress (pricedCustomerInfo pricedOrder')
               }
         in Just event
       NotSent -> Nothing

-- ---------------------------
-- Create events
-- ---------------------------

-- | Convert priced order line to shippable order line
makeShipmentLine :: PricedOrderLine -> Maybe ShippableOrderLine
makeShipmentLine (ProductLine line) = Just $ ShippableOrderLine
  { shippableProductCode = pricedProductCode line
  , shippableQuantity = pricedQuantity line
  }
makeShipmentLine (CommentLine _) = Nothing

-- | Create shipping event
createShippingEvent :: PricedOrder -> ShippableOrderPlaced
createShippingEvent placedOrder = ShippableOrderPlaced
  { shippableOrderId = pricedOrderId placedOrder
  , shippingAddress = pricedShippingAddress placedOrder
  , shipmentLines = mapMaybe makeShipmentLine (pricedLines placedOrder)
  , pdf = PdfAttachment
      { pdfName = printf "Order%s.pdf" (getOrderId (pricedOrderId placedOrder))
      , pdfBytes = []
      }
  }

-- | Create billing event if amount > 0
createBillingEvent :: PricedOrder -> Maybe BillableOrderPlaced
createBillingEvent placedOrder =
  let billingAmount = getBillingAmount (pricedAmountToBill placedOrder)
  in if billingAmount > 0
     then Just $ BillableOrderPlaced
            { billableOrderId = pricedOrderId placedOrder
            , billingAddress = pricedBillingAddress placedOrder
            , amountToBill = pricedAmountToBill placedOrder
            }
     else Nothing

-- | Helper to convert an Option into a List
listOfOption :: Maybe a -> [a]
listOfOption Nothing = []
listOfOption (Just x) = [x]

-- | Create all events from the order
createEvents :: CreateEvents
createEvents pricedOrder' acknowledgmentEventOpt =
  let acknowledgmentEvents = listOfOption $ fmap AcknowledgmentSentEvent acknowledgmentEventOpt
      shippingEvents = [ShippableOrderPlacedEvent $ createShippingEvent pricedOrder']
      billingEvents = listOfOption (BillableOrderPlacedEvent <$> createBillingEvent pricedOrder')
  in acknowledgmentEvents ++ shippingEvents ++ billingEvents

-- ---------------------------
-- Overall workflow
-- ---------------------------

-- | Main PlaceOrder workflow
placeOrder :: CheckProductCodeExists
           -> CheckAddressExists
           -> GetPricingFunction
           -> CalculateShippingCost
           -> CreateOrderAcknowledgmentLetter
           -> SendOrderAcknowledgment
           -> PlaceOrder
placeOrder checkProductExists checkAddressExists getPricingFunction' calculateShippingCost' createOrderAcknowledgmentLetter sendOrderAcknowledgment unvalidatedOrder =
  runExceptT $ do
    -- Validate order
    validatedOrder <- ExceptT $ asyncResultMapError ValidationErr $
      validateOrder checkProductExists checkAddressExists unvalidatedOrder

    -- Price order
    pricedOrder' <- ExceptT $ asyncResultMapError PricingErr $ asyncResultOfResult $
      priceOrder getPricingFunction' validatedOrder

    -- Add shipping info
    let pricedOrderWithShipping = addShippingInfoToOrder calculateShippingCost' pricedOrder'
        pricedOrderWithFreeVipShipping = freeVipShipping pricedOrderWithShipping

    -- Acknowledge order
    let acknowledgementOption = acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrderWithFreeVipShipping

    -- Create events
    let events = createEvents pricedOrder' acknowledgementOption

    return events

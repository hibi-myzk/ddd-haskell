{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module OrderTaking.PlaceOrder.Dto
  ( CustomerInfoDto(..)
  , toUnvalidatedCustomerInfo
  , toCustomerInfo
  , fromCustomerInfo
  , AddressDto(..)
  , toUnvalidatedAddress
  , toAddress
  , fromAddress
  , OrderFormLineDto(..)
  , toUnvalidatedOrderLine
  , PricedOrderLineDto(..)
  , fromPricedOrderLine
  , OrderFormDto(..)
  , toUnvalidatedOrder
  , ShippableOrderLineDto(..)
  , ShippableOrderPlacedDto(..)
  , fromShippableOrderLine
  , fromShippableOrderPlaced
  , BillableOrderPlacedDto(..)
  , fromBillableOrderPlaced
  , OrderAcknowledgmentSentDto(..)
  , fromOrderAcknowledgmentSent
  , PlaceOrderEventDto
  , PlaceOrderEventValue(..)
  , fromPlaceOrderEvent
  , PlaceOrderErrorDto(..)
  , fromPlaceOrderError
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import OrderTaking.Common.SimpleTypes
import OrderTaking.Common.CompoundTypes
import qualified OrderTaking.PlaceOrder.InternalTypes as InternalTypes
import qualified OrderTaking.PlaceOrder.PublicTypes as PublicTypes
import OrderTaking.PlaceOrder.InternalTypes (PricedOrderLine(..))
import OrderTaking.PlaceOrder.PublicTypes (UnvalidatedCustomerInfo(..), UnvalidatedAddress(..), UnvalidatedOrderLine(..), UnvalidatedOrder(..), ShippableOrderLine(..), ShippableOrderPlaced(..), BillableOrderPlaced(..), OrderAcknowledgmentSent(..), PlaceOrderEvent(..), PlaceOrderError(..), PricingError(..))
import OrderTaking.Result

-- ======================================================
-- This file contains the logic for working with data transfer objects (DTOs)
--
-- This represents the code in chapter 11, "Serialization"
--
-- Each type of DTO is defined using primitive, serializable types
-- and then there are `toDomain` and `fromDomain` functions defined for each DTO.
--
-- ======================================================

-- ==================================
-- DTOs for PlaceOrder workflow
-- ==================================

-- Helper function to get the value from a Maybe, and if Nothing, use the defaultValue
defaultIfNothing :: a -> Maybe a -> a
defaultIfNothing = fromMaybe

-- ===============================================
-- DTO for CustomerInfo
-- ===============================================

data CustomerInfoDto = CustomerInfoDto
  { customerFirstName :: String
  , customerLastName :: String
  , customerEmailAddress :: String
  , customerVipStatus :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CustomerInfoDto
instance FromJSON CustomerInfoDto

-- | Convert the DTO into a UnvalidatedCustomerInfo object.
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedCustomerInfo :: CustomerInfoDto -> UnvalidatedCustomerInfo
toUnvalidatedCustomerInfo CustomerInfoDto{..} = UnvalidatedCustomerInfo
  { unvalidatedFirstName = customerFirstName
  , unvalidatedLastName = customerLastName
  , unvalidatedEmailAddress = customerEmailAddress
  , unvalidatedVipStatus = customerVipStatus
  }

-- | Convert the DTO into a CustomerInfo object
-- Used when importing from the outside world into the domain, eg loading from a database
toCustomerInfo :: CustomerInfoDto -> Either ValidationError CustomerInfo
toCustomerInfo CustomerInfoDto{..} = do
  -- get each (validated) simple type from the DTO as a success or failure
  firstName <- createString50 "FirstName" customerFirstName
  lastName <- createString50 "LastName" customerLastName
  email <- createEmailAddress "EmailAddress" customerEmailAddress
  vipStatus <- createVipStatus "VipStatus" customerVipStatus
  -- combine the components to create the domain object
  let name = PersonalName firstName lastName
  let info = CustomerInfo name email vipStatus
  return info

-- | Convert a CustomerInfo object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromCustomerInfo :: CustomerInfo -> CustomerInfoDto
fromCustomerInfo (CustomerInfo (PersonalName firstName lastName) email vipStatus) =
  CustomerInfoDto
    { customerFirstName = getString50 firstName
    , customerLastName = getString50 lastName
    , customerEmailAddress = getEmailAddress email
    , customerVipStatus = getVipStatusString vipStatus
    }

-- ===============================================
-- DTO for Address
-- ===============================================

data AddressDto = AddressDto
  { addressLine1 :: String
  , addressLine2 :: String
  , addressLine3 :: String
  , addressLine4 :: String
  , addressCity :: String
  , addressZipCode :: String
  , addressState :: String
  , addressCountry :: String
  } deriving (Show, Eq, Generic)

instance ToJSON AddressDto
instance FromJSON AddressDto

-- | Convert the DTO into a UnvalidatedAddress
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedAddress :: AddressDto -> UnvalidatedAddress
toUnvalidatedAddress AddressDto{..} = UnvalidatedAddress
  { unvalidatedAddressLine1 = addressLine1
  , unvalidatedAddressLine2 = addressLine2
  , unvalidatedAddressLine3 = addressLine3
  , unvalidatedAddressLine4 = addressLine4
  , unvalidatedCity = addressCity
  , unvalidatedZipCode = addressZipCode
  , unvalidatedState = addressState
  , unvalidatedCountry = addressCountry
  }

-- | Convert the DTO into a Address object
-- Used when importing from the outside world into the domain, eg loading from a database.
toAddress :: AddressDto -> Either ValidationError Address
toAddress AddressDto{..} = do
  -- get each (validated) simple type from the DTO as a success or failure
  line1 <- createString50 "AddressLine1" addressLine1
  line2 <- createString50Option "AddressLine2" addressLine2
  line3 <- createString50Option "AddressLine3" addressLine3
  line4 <- createString50Option "AddressLine4" addressLine4
  city <- createString50 "City" addressCity
  zipCode <- createZipCode "ZipCode" addressZipCode
  state <- createUsStateCode "State" addressState
  country <- createString50 "Country" addressCountry

  -- combine the components to create the domain object
  let address = Address
        { addressLine1 = line1
        , addressLine2 = line2
        , addressLine3 = line3
        , addressLine4 = line4
        , city = city
        , zipCode = zipCode
        , state = state
        , country = country
        }
  return address

-- | Convert a Address object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromAddress :: Address -> AddressDto
fromAddress Address{..} = AddressDto
  { addressLine1 = getString50 addressLine1
  , addressLine2 = maybe "" getString50 addressLine2
  , addressLine3 = maybe "" getString50 addressLine3
  , addressLine4 = maybe "" getString50 addressLine4
  , addressCity = getString50 city
  , addressZipCode = getZipCode zipCode
  , addressState = getUsStateCode state
  , addressCountry = getString50 country
  }

-- ===============================================
-- DTOs for OrderLines
-- ===============================================

-- | From the order form used as input
data OrderFormLineDto = OrderFormLineDto
  { orderFormLineId :: String
  , orderFormProductCode :: String
  , orderFormQuantity :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON OrderFormLineDto
instance FromJSON OrderFormLineDto

-- | Convert the OrderFormLine into a UnvalidatedOrderLine
-- This always succeeds because there is no validation.
-- Used when importing an OrderForm from the outside world into the domain.
toUnvalidatedOrderLine :: OrderFormLineDto -> UnvalidatedOrderLine
toUnvalidatedOrderLine OrderFormLineDto{..} = UnvalidatedOrderLine
  { unvalidatedOrderLineId = orderFormLineId
  , unvalidatedProductCode = orderFormProductCode
  , unvalidatedQuantity = orderFormQuantity
  }

-- ===============================================
-- DTOs for PricedOrderLines
-- ===============================================

-- | Used in the output of the workflow
data PricedOrderLineDto = PricedOrderLineDto
  { pricedOrderLineId :: String
  , pricedProductCode :: String
  , pricedQuantity :: Double
  , pricedLinePrice :: Double
  , pricedComment :: String
  } deriving (Show, Eq)

-- | Convert a PricedOrderLine object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromPricedOrderLine :: PricedOrderLine -> PricedOrderLineDto
fromPricedOrderLine pricedLine = case pricedLine of
  ProductLine line -> PricedOrderLineDto
    { pricedOrderLineId = getOrderLineId (InternalTypes.pricedOrderLineId line)
    , pricedProductCode = getProductCode (InternalTypes.pricedProductCode line)
    , pricedQuantity = getOrderQuantityValue (InternalTypes.pricedQuantity line)
    , pricedLinePrice = getPrice (InternalTypes.pricedLinePrice line)
    , pricedComment = ""
    }
  CommentLine comment -> PricedOrderLineDto
    { pricedOrderLineId = ""
    , pricedProductCode = ""
    , pricedQuantity = 0.0
    , pricedLinePrice = 0.0
    , pricedComment = comment
    }

-- ===============================================
-- DTO for OrderForm
-- ===============================================

data OrderFormDto = OrderFormDto
  { orderFormOrderId :: String
  , orderFormCustomerInfo :: CustomerInfoDto
  , orderFormShippingAddress :: AddressDto
  , orderFormBillingAddress :: AddressDto
  , orderFormLines :: [OrderFormLineDto]
  , orderFormPromotionCode :: String
  } deriving (Show, Eq, Generic)

instance ToJSON OrderFormDto
instance FromJSON OrderFormDto

-- | Convert the OrderForm into a UnvalidatedOrder
-- This always succeeds because there is no validation.
toUnvalidatedOrder :: OrderFormDto -> UnvalidatedOrder
toUnvalidatedOrder OrderFormDto{..} = UnvalidatedOrder
  { unvalidatedOrderId = orderFormOrderId
  , unvalidatedCustomerInfo = toUnvalidatedCustomerInfo orderFormCustomerInfo
  , unvalidatedShippingAddress = toUnvalidatedAddress orderFormShippingAddress
  , unvalidatedBillingAddress = toUnvalidatedAddress orderFormBillingAddress
  , unvalidatedLines = map toUnvalidatedOrderLine orderFormLines
  , unvalidatedPromotionCode = orderFormPromotionCode
  }

-- ===============================================
-- DTO for ShippableOrderPlaced event
-- ===============================================

data ShippableOrderLineDto = ShippableOrderLineDto
  { shippableProductCode :: String
  , shippableQuantity :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON ShippableOrderLineDto
instance FromJSON ShippableOrderLineDto

-- | Event to send to shipping context
data ShippableOrderPlacedDto = ShippableOrderPlacedDto
  { shippableOrderId :: String
  , shippableShippingAddress :: AddressDto
  , shippableShipmentLines :: [ShippableOrderLineDto]
  , shippablePdf :: PdfAttachment
  } deriving (Show, Eq, Generic)

instance ToJSON ShippableOrderPlacedDto
instance FromJSON ShippableOrderPlacedDto

fromShippableOrderLine :: ShippableOrderLine -> ShippableOrderLineDto
fromShippableOrderLine line = ShippableOrderLineDto
  { shippableProductCode = getProductCode (PublicTypes.shippableProductCode line)
  , shippableQuantity = getOrderQuantityValue (PublicTypes.shippableQuantity line)
  }

-- | Convert a ShippableOrderPlaced object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromShippableOrderPlaced :: ShippableOrderPlaced -> ShippableOrderPlacedDto
fromShippableOrderPlaced event = ShippableOrderPlacedDto
  { shippableOrderId = getOrderId (PublicTypes.shippableOrderId event)
  , shippableShippingAddress = fromAddress (PublicTypes.shippingAddress event)
  , shippableShipmentLines = map fromShippableOrderLine (PublicTypes.shipmentLines event)
  , shippablePdf = PublicTypes.pdf event
  }

-- ===============================================
-- DTO for BillableOrderPlaced event
-- ===============================================

-- | Event to send to billing context
data BillableOrderPlacedDto = BillableOrderPlacedDto
  { billableOrderId :: String
  , billableBillingAddress :: AddressDto
  , billableAmountToBill :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON BillableOrderPlacedDto
instance FromJSON BillableOrderPlacedDto

-- | Convert a BillableOrderPlaced object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromBillableOrderPlaced :: BillableOrderPlaced -> BillableOrderPlacedDto
fromBillableOrderPlaced event = BillableOrderPlacedDto
  { billableOrderId = getOrderId (PublicTypes.billableOrderId event)
  , billableBillingAddress = fromAddress (PublicTypes.billingAddress event)
  , billableAmountToBill = getBillingAmount (PublicTypes.amountToBill event)
  }

-- ===============================================
-- DTO for OrderAcknowledgmentSent event
-- ===============================================

-- | Event to send to other bounded contexts
data OrderAcknowledgmentSentDto = OrderAcknowledgmentSentDto
  { acknowledgmentOrderId :: String
  , acknowledgmentEmailAddress :: String
  } deriving (Show, Eq, Generic)

instance ToJSON OrderAcknowledgmentSentDto
instance FromJSON OrderAcknowledgmentSentDto

-- | Convert a OrderAcknowledgmentSent object into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromOrderAcknowledgmentSent :: OrderAcknowledgmentSent -> OrderAcknowledgmentSentDto
fromOrderAcknowledgmentSent event = OrderAcknowledgmentSentDto
  { acknowledgmentOrderId = getOrderId (ackOrderId event)
  , acknowledgmentEmailAddress = getEmailAddress (ackEmailAddress event)
  }

-- ===============================================
-- DTO for PlaceOrderEvent
-- ===============================================

-- | Use a Map representation of a PlaceOrderEvent, suitable for JSON
-- See "Serializing Records and Choice Types Using Maps" in chapter 11
type PlaceOrderEventDto = Map String PlaceOrderEventValue

data PlaceOrderEventValue
  = ShippableOrderPlacedValue ShippableOrderPlacedDto
  | BillableOrderPlacedValue BillableOrderPlacedDto
  | OrderAcknowledgmentSentValue OrderAcknowledgmentSentDto
  deriving (Show, Eq, Generic)

instance ToJSON PlaceOrderEventValue
instance FromJSON PlaceOrderEventValue

-- | Convert a PlaceOrderEvent into the corresponding DTO.
-- Used when exporting from the domain to the outside world.
fromPlaceOrderEvent :: PlaceOrderEvent -> PlaceOrderEventDto
fromPlaceOrderEvent event = case event of
  ShippableOrderPlacedEvent shippableOrderPlaced ->
    let value = ShippableOrderPlacedValue (fromShippableOrderPlaced shippableOrderPlaced)
    in Map.singleton "ShippableOrderPlaced" value
  BillableOrderPlacedEvent billableOrderPlaced ->
    let value = BillableOrderPlacedValue (fromBillableOrderPlaced billableOrderPlaced)
    in Map.singleton "BillableOrderPlaced" value
  AcknowledgmentSentEvent orderAcknowledgmentSent ->
    let value = OrderAcknowledgmentSentValue (fromOrderAcknowledgmentSent orderAcknowledgmentSent)
    in Map.singleton "OrderAcknowledgmentSent" value

-- ===============================================
-- DTO for PlaceOrderError
-- ===============================================

data PlaceOrderErrorDto = PlaceOrderErrorDto
  { errorCode :: String
  , errorMessage :: String
  } deriving (Show, Eq, Generic)

instance ToJSON PlaceOrderErrorDto
instance FromJSON PlaceOrderErrorDto

fromPlaceOrderError :: PlaceOrderError -> PlaceOrderErrorDto
fromPlaceOrderError err = case err of
  ValidationErr (ValidationError msg) -> PlaceOrderErrorDto
    { errorCode = "ValidationError"
    , errorMessage = msg
    }
  PricingErr (PricingError msg) -> PlaceOrderErrorDto
    { errorCode = "PricingError"
    , errorMessage = msg
    }
  RemoteServiceErr serviceError -> PlaceOrderErrorDto
    { errorCode = "RemoteServiceError"
    , errorMessage = PublicTypes.serviceName (PublicTypes.service serviceError) ++ ": " ++ PublicTypes.exception serviceError
    }

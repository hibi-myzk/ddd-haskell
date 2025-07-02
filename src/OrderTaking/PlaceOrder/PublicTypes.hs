-- | Public types for the PlaceOrder workflow
-- Converted from F# PlaceOrder.PublicTypes.fs
module OrderTaking.PlaceOrder.PublicTypes
  ( -- * Input types (unvalidated)
    UnvalidatedCustomerInfo(..)
  , UnvalidatedAddress(..)
  , UnvalidatedOrderLine(..)
  , UnvalidatedOrder(..)
    -- * Output types (success case)
  , OrderAcknowledgmentSent(..)
  , OrderPlaced
  , ShippableOrderLine(..)
  , ShippableOrderPlaced(..)
  , BillableOrderPlaced(..)
  , PlaceOrderEvent(..)
    -- * Error types
  , ValidationError(..)
  , PricingError(..)
  , ServiceInfo(..)
  , RemoteServiceError(..)
  , PlaceOrderError(..)
    -- * The workflow itself
  , PlaceOrder
  ) where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Common.CompoundTypes
import OrderTaking.Result (AsyncResult, ValidationError(..))

-- | Unvalidated customer information from input
data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
  { unvalidatedFirstName :: String
  , unvalidatedLastName :: String
  , unvalidatedEmailAddress :: String
  , unvalidatedVipStatus :: String
  } deriving (Show, Eq)

-- | Unvalidated address from input
data UnvalidatedAddress = UnvalidatedAddress
  { unvalidatedAddressLine1 :: String
  , unvalidatedAddressLine2 :: String
  , unvalidatedAddressLine3 :: String
  , unvalidatedAddressLine4 :: String
  , unvalidatedCity :: String
  , unvalidatedZipCode :: String
  , unvalidatedState :: String
  , unvalidatedCountry :: String
  } deriving (Show, Eq)

-- | Unvalidated order line from input
data UnvalidatedOrderLine = UnvalidatedOrderLine
  { unvalidatedOrderLineId :: String
  , unvalidatedProductCode :: String
  , unvalidatedQuantity :: Double
  } deriving (Show, Eq)

-- | Unvalidated order from input
data UnvalidatedOrder = UnvalidatedOrder
  { unvalidatedOrderId :: String
  , unvalidatedCustomerInfo :: UnvalidatedCustomerInfo
  , unvalidatedShippingAddress :: UnvalidatedAddress
  , unvalidatedBillingAddress :: UnvalidatedAddress
  , unvalidatedLines :: [UnvalidatedOrderLine]
  , unvalidatedPromotionCode :: String
  } deriving (Show, Eq)

-- | Event created if the Acknowledgment was successfully posted
data OrderAcknowledgmentSent = OrderAcknowledgmentSent
  { ackOrderId :: OrderId
  , ackEmailAddress :: EmailAddress
  } deriving (Show, Eq)

-- | Event to send to shipping context (alias for PricedOrder)
type OrderPlaced = PricedOrder

-- | Shippable order line
data ShippableOrderLine = ShippableOrderLine
  { shippableProductCode :: ProductCode
  , shippableQuantity :: OrderQuantity
  } deriving (Show, Eq)

-- | Event to send to shipping context
data ShippableOrderPlaced = ShippableOrderPlaced
  { shippableOrderId :: OrderId
  , shippingAddress :: Address
  , shipmentLines :: [ShippableOrderLine]
  , pdf :: PdfAttachment
  } deriving (Show, Eq)

-- | Event to send to billing context
data BillableOrderPlaced = BillableOrderPlaced
  { billableOrderId :: OrderId
  , billingAddress :: Address
  , amountToBill :: BillingAmount
  } deriving (Show, Eq)

-- | The possible events resulting from the PlaceOrder workflow
data PlaceOrderEvent
  = ShippableOrderPlacedEvent ShippableOrderPlaced
  | BillableOrderPlacedEvent BillableOrderPlaced
  | AcknowledgmentSentEvent OrderAcknowledgmentSent
  deriving (Show, Eq)

-- | Pricing error
newtype PricingError = PricingError String
  deriving (Show, Eq)

-- | Service information for remote calls
data ServiceInfo = ServiceInfo
  { serviceName :: String
  , serviceEndpoint :: String  -- Simplified from URI
  } deriving (Show, Eq)

-- | Remote service error
data RemoteServiceError = RemoteServiceError
  { service :: ServiceInfo
  , exception :: String  -- Simplified from System.Exception
  } deriving (Show, Eq)

-- | All possible errors from PlaceOrder workflow
data PlaceOrderError
  = ValidationErr ValidationError
  | PricingErr PricingError
  | RemoteServiceErr RemoteServiceError
  deriving (Show, Eq)

-- | Forward declaration - will be defined in InternalTypes
data PricedOrder = PricedOrder
  { pricedOrderId :: OrderId
  , pricedCustomerInfo :: CustomerInfo
  , pricedShippingAddress :: Address
  , pricedBillingAddress :: Address
  , pricedAmountToBill :: BillingAmount
  , pricedLines :: [PricedOrderLine]
  , pricedPricingMethod :: PricingMethod
  } deriving (Show, Eq)

-- Forward declarations for types defined in InternalTypes
data PricedOrderLine = PricedOrderLine deriving (Show, Eq)
data PricingMethod = PricingMethod deriving (Show, Eq)

-- | The PlaceOrder workflow function type
type PlaceOrder = UnvalidatedOrder -> AsyncResult PlaceOrderError [PlaceOrderEvent]

-- | Internal types for the PlaceOrder workflow
-- Converted from F# PlaceOrder.InternalTypes.fs
module OrderTaking.PlaceOrder.InternalTypes
  ( -- * Validation step types
    CheckProductCodeExists
  , AddressValidationError(..)
  , CheckedAddress(..)
  , CheckAddressExists
    -- * Validated Order types
  , PricingMethod(..)
  , ValidatedOrderLine(..)
  , ValidatedOrder(..)
  , ValidateOrder
    -- * Pricing step types
  , GetProductPrice
  , TryGetProductPrice
  , GetPricingFunction
  , GetStandardPrices
  , GetPromotionPrices
  , PricedOrderProductLine(..)
  , PricedOrderLine(..)
  , PricedOrder(..)
  , PriceOrder
    -- * Shipping types
  , ShippingMethod(..)
  , ShippingInfo(..)
  , PricedOrderWithShippingMethod(..)
  , CalculateShippingCost
  , AddShippingInfoToOrder
  , FreeVipShipping
    -- * Acknowledgment types
  , HtmlString(..)
  , OrderAcknowledgment(..)
  , CreateOrderAcknowledgmentLetter
  , SendResult(..)
  , SendOrderAcknowledgment
  , AcknowledgeOrder
    -- * Event creation
  , CreateEvents
  ) where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Common.CompoundTypes
import OrderTaking.Result (AsyncResult, ValidationError(..))
import OrderTaking.PlaceOrder.PublicTypes (UnvalidatedAddress, UnvalidatedOrder, PlaceOrderEvent, OrderAcknowledgmentSent, PricingError)

-- | Product validation function type
type CheckProductCodeExists = ProductCode -> Bool

-- | Address validation errors
data AddressValidationError = InvalidFormat | AddressNotFound
  deriving (Show, Eq)

-- | Checked address wrapper
newtype CheckedAddress = CheckedAddress UnvalidatedAddress
  deriving (Show, Eq)

-- | Address validation function type
type CheckAddressExists = UnvalidatedAddress -> AsyncResult AddressValidationError CheckedAddress

-- | Pricing method
data PricingMethod = Standard | Promotion PromotionCode
  deriving (Show, Eq)

-- | Validated order line
data ValidatedOrderLine = ValidatedOrderLine
  { validatedOrderLineId :: OrderLineId
  , validatedProductCode :: ProductCode
  , validatedQuantity :: OrderQuantity
  } deriving (Show, Eq)

-- | Validated order
data ValidatedOrder = ValidatedOrder
  { validatedOrderId :: OrderId
  , validatedCustomerInfo :: CustomerInfo
  , validatedShippingAddress :: Address
  , validatedBillingAddress :: Address
  , validatedLines :: [ValidatedOrderLine]
  , validatedPricingMethod :: PricingMethod
  } deriving (Show, Eq)

-- | Validation function type
type ValidateOrder = CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder -> AsyncResult ValidationError ValidatedOrder

-- | Product price lookup function
type GetProductPrice = ProductCode -> Price

-- | Optional product price lookup function
type TryGetProductPrice = ProductCode -> Maybe Price

-- | Pricing function factory
type GetPricingFunction = PricingMethod -> GetProductPrice

-- | Standard prices function
type GetStandardPrices = () -> GetProductPrice

-- | Promotional prices function
type GetPromotionPrices = PromotionCode -> TryGetProductPrice

-- | Priced order product line
data PricedOrderProductLine = PricedOrderProductLine
  { pricedOrderLineId :: OrderLineId
  , pricedProductCode :: ProductCode
  , pricedQuantity :: OrderQuantity
  , pricedLinePrice :: Price
  } deriving (Show, Eq)

-- | Priced order line (can be product or comment)
data PricedOrderLine
  = ProductLine PricedOrderProductLine
  | CommentLine String
  deriving (Show, Eq)

-- | Priced order
data PricedOrder = PricedOrder
  { pricedOrderId :: OrderId
  , pricedCustomerInfo :: CustomerInfo
  , pricedShippingAddress :: Address
  , pricedBillingAddress :: Address
  , pricedAmountToBill :: BillingAmount
  , pricedLines :: [PricedOrderLine]
  , pricedPricingMethod :: PricingMethod
  } deriving (Show, Eq)

-- | Pricing function type
type PriceOrder = GetPricingFunction -> ValidatedOrder -> Either PricingError PricedOrder

-- | Shipping methods
data ShippingMethod = PostalService | Fedex24 | Fedex48 | Ups48
  deriving (Show, Eq)

-- | Shipping information
data ShippingInfo = ShippingInfo
  { shippingMethod :: ShippingMethod
  , shippingCost :: Price
  } deriving (Show, Eq)

-- | Priced order with shipping method
data PricedOrderWithShippingMethod = PricedOrderWithShippingMethod
  { shippingInfo :: ShippingInfo
  , pricedOrder :: PricedOrder
  } deriving (Show, Eq)

-- | Shipping cost calculation function
type CalculateShippingCost = PricedOrder -> Price

-- | Add shipping info function
type AddShippingInfoToOrder = CalculateShippingCost -> PricedOrder -> PricedOrderWithShippingMethod

-- | VIP shipping function
type FreeVipShipping = PricedOrderWithShippingMethod -> PricedOrderWithShippingMethod

-- | HTML string wrapper
newtype HtmlString = HtmlString String
  deriving (Show, Eq)

-- | Order acknowledgment
data OrderAcknowledgment = OrderAcknowledgment
  { ackEmailAddress :: EmailAddress
  , ackLetter :: HtmlString
  } deriving (Show, Eq)

-- | Create acknowledgment letter function
type CreateOrderAcknowledgmentLetter = PricedOrderWithShippingMethod -> HtmlString

-- | Send result
data SendResult = Sent | NotSent
  deriving (Show, Eq)

-- | Send acknowledgment function
type SendOrderAcknowledgment = OrderAcknowledgment -> SendResult

-- | Acknowledge order function
type AcknowledgeOrder = CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment -> PricedOrderWithShippingMethod -> Maybe OrderAcknowledgmentSent

-- | Create events function
type CreateEvents = PricedOrder -> Maybe OrderAcknowledgmentSent -> [PlaceOrderEvent]

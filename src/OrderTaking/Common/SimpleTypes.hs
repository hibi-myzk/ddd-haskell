{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple types and constrained types related to the OrderTaking domain
-- Converted from F# Common.SimpleTypes.fs
module OrderTaking.Common.SimpleTypes
  ( -- * Basic constrained types
    String50(..)
  , EmailAddress(..)
  , VipStatus(..)
  , ZipCode(..)
  , UsStateCode(..)
  , OrderId(..)
  , OrderLineId(..)
  , WidgetCode(..)
  , GizmoCode(..)
  , ProductCode(..)
  , UnitQuantity(..)
  , KilogramQuantity(..)
  , OrderQuantity(..)
  , Price(..)
  , BillingAmount(..)
  , PdfAttachment(..)
  , PromotionCode(..)
    -- * Smart constructors
  , createString50
  , createString50Option
  , createEmailAddress
  , createVipStatus
  , createZipCode
  , createUsStateCode
  , createOrderId
  , createOrderLineId
  , createWidgetCode
  , createGizmoCode
  , createProductCode
  , createUnitQuantity
  , createKilogramQuantity
  , createOrderQuantity
  , createPrice
  , createBillingAmount
  , unsafeCreatePrice
  , multiplyPrice
  , sumPrices
    -- * Value extractors
  , getString50
  , getEmailAddress
  , getVipStatusString
  , getZipCode
  , getUsStateCode
  , getOrderId
  , getOrderLineId
  , getWidgetCode
  , getGizmoCode
  , getProductCode
  , getUnitQuantity
  , getKilogramQuantity
  , getOrderQuantityValue
  , getPrice
  , getBillingAmount
  ) where

import OrderTaking.Result (ValidationError(..), Result)
import OrderTaking.Common.ConstrainedTypes
import Data.Word (Word8)

-- | Constrained to be 50 chars or less, not null
newtype String50 = String50 String
  deriving (Show, Eq)

-- | An email address
newtype EmailAddress = EmailAddress String
  deriving (Show, Eq)

-- | Customer's VIP status
data VipStatus = Normal | Vip
  deriving (Show, Eq)

-- | A zip code
newtype ZipCode = ZipCode String
  deriving (Show, Eq)

-- | A US 2 letter state code
newtype UsStateCode = UsStateCode String
  deriving (Show, Eq)

-- | An Id for Orders. Constrained to be a non-empty string < 50 chars
newtype OrderId = OrderId String
  deriving (Show, Eq)

-- | An Id for OrderLines. Constrained to be a non-empty string < 50 chars
newtype OrderLineId = OrderLineId String
  deriving (Show, Eq)

-- | The codes for Widgets start with a "W" and then four digits
newtype WidgetCode = WidgetCode String
  deriving (Show, Eq)

-- | The codes for Gizmos start with a "G" and then three digits
newtype GizmoCode = GizmoCode String
  deriving (Show, Eq)

-- | A ProductCode is either a Widget or a Gizmo
data ProductCode = Widget WidgetCode | Gizmo GizmoCode
  deriving (Show, Eq)

-- | Constrained to be an integer between 1 and 1000
newtype UnitQuantity = UnitQuantity Int
  deriving (Show, Eq)

-- | Constrained to be a decimal between 0.05 and 100.00
newtype KilogramQuantity = KilogramQuantity Double
  deriving (Show, Eq)

-- | A Quantity is either a Unit or a Kilogram
data OrderQuantity = Unit UnitQuantity | Kilogram KilogramQuantity
  deriving (Show, Eq)

-- | Constrained to be a decimal between 0.0 and 1000.00
newtype Price = Price Double
  deriving (Show, Eq)

-- | Constrained to be a decimal between 0.0 and 10000.00
newtype BillingAmount = BillingAmount Double
  deriving (Show, Eq)

-- | Represents a PDF attachment
data PdfAttachment = PdfAttachment
  { pdfName :: String
  , pdfBytes :: [Word8]
  } deriving (Show, Eq)

-- | Promotion code
newtype PromotionCode = PromotionCode String
  deriving (Show, Eq)

-- Smart Constructors

-- | Create a String50 from a string
createString50 :: String -> String -> Result ValidationError String50
createString50 fieldName = createString fieldName String50 50

-- | Create an optional String50 from a string
createString50Option :: String -> String -> Result ValidationError (Maybe String50)
createString50Option fieldName = createStringOption fieldName String50 50

-- | Create an EmailAddress from a string
createEmailAddress :: String -> String -> Result ValidationError EmailAddress
createEmailAddress fieldName = createLike fieldName EmailAddress ".+@.+"

-- | Create a VipStatus from a string
createVipStatus :: String -> String -> Result ValidationError VipStatus
createVipStatus fieldName str = case str of
  "normal" -> Right Normal
  "Normal" -> Right Normal
  "vip" -> Right Vip
  "VIP" -> Right Vip
  "Vip" -> Right Vip
  _ -> Left $ ValidationError $ fieldName ++ ": Must be one of 'Normal', 'VIP'"

-- | Create a ZipCode from a string
createZipCode :: String -> String -> Result ValidationError ZipCode
createZipCode fieldName = createLike fieldName ZipCode "\\d{5}"

-- | Create a UsStateCode from a string
createUsStateCode :: String -> String -> Result ValidationError UsStateCode
createUsStateCode fieldName str
  | str `elem` validStates = Right (UsStateCode str)
  | otherwise = Left $ ValidationError $ fieldName ++ ": Invalid US state code"
  where
    validStates = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                   "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                   "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                   "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"]

-- | Create an OrderId from a string
createOrderId :: String -> String -> Result ValidationError OrderId
createOrderId fieldName = createString fieldName OrderId 50

-- | Create an OrderLineId from a string
createOrderLineId :: String -> String -> Result ValidationError OrderLineId
createOrderLineId fieldName = createString fieldName OrderLineId 50

-- | Create a WidgetCode from a string
createWidgetCode :: String -> String -> Result ValidationError WidgetCode
createWidgetCode fieldName = createLike fieldName WidgetCode "W\\d{4}"

-- | Create a GizmoCode from a string
createGizmoCode :: String -> String -> Result ValidationError GizmoCode
createGizmoCode fieldName = createLike fieldName GizmoCode "G\\d{3}"

-- | Create a ProductCode from a string
createProductCode :: String -> String -> Result ValidationError ProductCode
createProductCode fieldName code
  | null code = Left $ ValidationError $ fieldName ++ ": Must not be null or empty"
  | head code == 'W' = fmap Widget (createWidgetCode fieldName code)
  | head code == 'G' = fmap Gizmo (createGizmoCode fieldName code)
  | otherwise = Left $ ValidationError $ fieldName ++ ": Format not recognized '" ++ code ++ "'"

-- | Create a UnitQuantity from an int
createUnitQuantity :: String -> Int -> Result ValidationError UnitQuantity
createUnitQuantity fieldName = createInt fieldName UnitQuantity 1 1000

-- | Create a KilogramQuantity from a decimal
createKilogramQuantity :: String -> Double -> Result ValidationError KilogramQuantity
createKilogramQuantity fieldName = createDecimal fieldName KilogramQuantity 0.05 100.0

-- | Create an OrderQuantity from a productCode and quantity
createOrderQuantity :: String -> ProductCode -> Double -> Result ValidationError OrderQuantity
createOrderQuantity fieldName productCode quantity = case productCode of
  Widget _ -> fmap Unit (createUnitQuantity fieldName (round quantity))
  Gizmo _ -> fmap Kilogram (createKilogramQuantity fieldName quantity)

-- | Create a Price from a decimal
createPrice :: Double -> Result ValidationError Price
createPrice = createDecimal "Price" Price 0.0 1000.0

-- | Create a Price from a decimal, throwing an exception if out of bounds
unsafeCreatePrice :: Double -> Price
unsafeCreatePrice v = case createPrice v of
  Right price -> price
  Left (ValidationError err) -> error $ "Not expecting Price to be out of bounds: " ++ err

-- | Multiply a Price by a decimal qty
multiplyPrice :: Double -> Price -> Result ValidationError Price
multiplyPrice qty (Price p) = createPrice (qty * p)

-- | Create a BillingAmount from a decimal
createBillingAmount :: Double -> Result ValidationError BillingAmount
createBillingAmount = createDecimal "BillingAmount" BillingAmount 0.0 10000.0

-- | Sum a list of prices to make a billing amount
sumPrices :: [Price] -> Result ValidationError BillingAmount
sumPrices prices = 
  let total = sum [p | Price p <- prices]
  in createBillingAmount total

-- Value Extractors

-- | Return the value inside a String50
getString50 :: String50 -> String
getString50 (String50 str) = str

-- | Return the string value inside an EmailAddress
getEmailAddress :: EmailAddress -> String
getEmailAddress (EmailAddress str) = str

-- | Return a string representation of VipStatus
getVipStatusString :: VipStatus -> String
getVipStatusString Normal = "Normal"
getVipStatusString Vip = "VIP"

-- | Return the string value inside a ZipCode
getZipCode :: ZipCode -> String
getZipCode (ZipCode str) = str

-- | Return the string value inside a UsStateCode
getUsStateCode :: UsStateCode -> String
getUsStateCode (UsStateCode str) = str

-- | Return the string value inside an OrderId
getOrderId :: OrderId -> String
getOrderId (OrderId str) = str

-- | Return the string value inside an OrderLineId
getOrderLineId :: OrderLineId -> String
getOrderLineId (OrderLineId str) = str

-- | Return the string value inside a WidgetCode
getWidgetCode :: WidgetCode -> String
getWidgetCode (WidgetCode code) = code

-- | Return the string value inside a GizmoCode
getGizmoCode :: GizmoCode -> String
getGizmoCode (GizmoCode code) = code

-- | Return the string value inside a ProductCode
getProductCode :: ProductCode -> String
getProductCode (Widget (WidgetCode wc)) = wc
getProductCode (Gizmo (GizmoCode gc)) = gc

-- | Return the value inside a UnitQuantity
getUnitQuantity :: UnitQuantity -> Int
getUnitQuantity (UnitQuantity v) = v

-- | Return the value inside a KilogramQuantity
getKilogramQuantity :: KilogramQuantity -> Double
getKilogramQuantity (KilogramQuantity v) = v

-- | Return the value inside an OrderQuantity
getOrderQuantityValue :: OrderQuantity -> Double
getOrderQuantityValue (Unit uq) = fromIntegral (getUnitQuantity uq)
getOrderQuantityValue (Kilogram kq) = getKilogramQuantity kq

-- | Return the value inside a Price
getPrice :: Price -> Double
getPrice (Price v) = v

-- | Return the value inside a BillingAmount
getBillingAmount :: BillingAmount -> Double
getBillingAmount (BillingAmount v) = v

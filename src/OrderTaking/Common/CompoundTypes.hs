-- | Common compound types used throughout the OrderTaking domain
-- Converted from F# Common.CompoundTypes.fs
module OrderTaking.Common.CompoundTypes
  ( -- * Customer-related types
    PersonalName(..)
  , CustomerInfo(..)
    -- * Address-related types
  , Address(..)
  ) where

import OrderTaking.Common.SimpleTypes

-- | Personal name with first and last name
data PersonalName = PersonalName
  { firstName :: String50
  , lastName :: String50
  } deriving (Show, Eq)

-- | Customer information
data CustomerInfo = CustomerInfo
  { customerName :: PersonalName
  , emailAddress :: EmailAddress
  , vipStatus :: VipStatus
  } deriving (Show, Eq)

-- | Address information
data Address = Address
  { addressLine1 :: String50
  , addressLine2 :: Maybe String50
  , addressLine3 :: Maybe String50
  , addressLine4 :: Maybe String50
  , city :: String50
  , zipCode :: ZipCode
  , state :: UsStateCode
  , country :: String50
  } deriving (Show, Eq)

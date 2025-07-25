{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for creating constrained types with validation
-- Converted from F# Common.SimpleTypes.fs ConstrainedType module
module OrderTaking.Common.ConstrainedTypes
  ( createString
  , createStringOption
  , createInt
  , createDecimal
  , createLike
  ) where

import OrderTaking.Result (ValidationError(..), Result)
import Text.Regex.TDFA ((=~))

-- | Create a constrained string using the constructor provided
-- Return Error if input is null, empty, or length > maxLen
createString :: String -> (String -> a) -> Int -> String -> Result ValidationError a
createString fieldName ctor maxLen str
  | null str = Left $ ValidationError $ fieldName ++ " must not be null or empty"
  | length str > maxLen = Left $ ValidationError $ fieldName ++ " must not be more than " ++ show maxLen ++ " chars"
  | otherwise = Right (ctor str)

-- | Create an optional constrained string using the constructor provided
-- Return Nothing if input is null, empty.
-- Return error if length > maxLen
-- Return Just if the input is valid
createStringOption :: String -> (String -> a) -> Int -> String -> Result ValidationError (Maybe a)
createStringOption fieldName ctor maxLen str
  | null str = Right Nothing
  | length str > maxLen = Left $ ValidationError $ fieldName ++ " must not be more than " ++ show maxLen ++ " chars"
  | otherwise = Right (Just (ctor str))

-- | Create a constrained integer using the constructor provided
-- Return Error if input is less than minVal or more than maxVal
createInt :: String -> (Int -> a) -> Int -> Int -> Int -> Result ValidationError a
createInt fieldName ctor minVal maxVal i
  | i < minVal = Left $ ValidationError $ fieldName ++ ": Must not be less than " ++ show minVal
  | i > maxVal = Left $ ValidationError $ fieldName ++ ": Must not be greater than " ++ show maxVal
  | otherwise = Right (ctor i)

-- | Create a constrained decimal using the constructor provided
-- Return Error if input is less than minVal or more than maxVal
createDecimal :: String -> (Double -> a) -> Double -> Double -> Double -> Result ValidationError a
createDecimal fieldName ctor minVal maxVal d
  | d < minVal = Left $ ValidationError $ fieldName ++ ": Must not be less than " ++ show minVal
  | d > maxVal = Left $ ValidationError $ fieldName ++ ": Must not be greater than " ++ show maxVal
  | otherwise = Right (ctor d)

-- | Create a constrained string using the constructor provided
-- Return Error if input is null, empty, or does not match the regex pattern
createLike :: String -> (String -> a) -> String -> String -> Result ValidationError a
createLike fieldName ctor pattern str
  | null str = Left $ ValidationError $ fieldName ++ ": Must not be null or empty"
  | str =~ pattern = Right (ctor str)
  | otherwise = Left $ ValidationError $ fieldName ++ ": '" ++ str ++ "' must match the pattern '" ++ pattern ++ "'"

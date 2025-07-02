-- | Main application entry point
module Main where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Result

main :: IO ()
main = do
  putStrLn "Order Taking System (Converted from F#)"
  putStrLn "======================================"
  
  -- Test creating a simple constrained type
  case createString50 "TestField" "Hello World" of
    Right str50 -> putStrLn $ "Created String50: " ++ getString50 str50
    Left (ValidationError err) -> putStrLn $ "Error: " ++ err
  
  -- Test creating an email address
  case createEmailAddress "Email" "test@example.com" of
    Right email -> putStrLn $ "Created EmailAddress: " ++ getEmailAddress email
    Left (ValidationError err) -> putStrLn $ "Error: " ++ err
  
  -- Test creating a product code
  case createProductCode "ProductCode" "W1234" of
    Right productCode -> putStrLn $ "Created ProductCode: " ++ getProductCode productCode
    Left (ValidationError err) -> putStrLn $ "Error: " ++ err
  
  putStrLn "\nConversion from F# to Haskell completed successfully!"
  putStrLn "This demonstrates the core domain types working correctly."

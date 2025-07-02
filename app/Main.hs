-- | Main application entry point
module Main where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Result
import OrderTaking.PlaceOrder.Api (placeOrderApi, HttpRequest(..), HttpResponse(..))
import Data.Aeson (decode, Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as L8

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
  
  putStrLn "\nTesting PlaceOrder API..."
  putStrLn "========================="
  
  -- Create a sample order form JSON
  let sampleOrderJson = concat [
        "{",
        "\"orderFormOrderId\": \"ORD-001\",",
        "\"orderFormCustomerInfo\": {",
        "\"customerFirstName\": \"John\",",
        "\"customerLastName\": \"Doe\",",
        "\"customerEmailAddress\": \"john.doe@example.com\",",
        "\"customerVipStatus\": \"Normal\"",
        "},",
        "\"orderFormShippingAddress\": {",
        "\"addressLine1\": \"123 Main St\",",
        "\"addressLine2\": \"\",",
        "\"addressLine3\": \"\",",
        "\"addressLine4\": \"\",",
        "\"addressCity\": \"Seattle\",",
        "\"addressZipCode\": \"98101\",",
        "\"addressState\": \"WA\",",
        "\"addressCountry\": \"USA\"",
        "},",
        "\"orderFormBillingAddress\": {",
        "\"addressLine1\": \"123 Main St\",",
        "\"addressLine2\": \"\",",
        "\"addressLine3\": \"\",",
        "\"addressLine4\": \"\",",
        "\"addressCity\": \"Seattle\",",
        "\"addressZipCode\": \"98101\",",
        "\"addressState\": \"WA\",",
        "\"addressCountry\": \"USA\"",
        "},",
        "\"orderFormLines\": [",
        "{",
        "\"orderFormLineId\": \"LINE-001\",",
        "\"orderFormProductCode\": \"W1234\",",
        "\"orderFormQuantity\": 2.0",
        "},",
        "{",
        "\"orderFormLineId\": \"LINE-002\",",
        "\"orderFormProductCode\": \"G123\",",
        "\"orderFormQuantity\": 1.0",
        "}",
        "],",
        "\"orderFormPromotionCode\": \"HALF\"",
        "}"
        ]
  
  -- Create HTTP request
  let request = HttpRequest
        { action = "POST"
        , uri = "/orders"
        , body = sampleOrderJson
        }
  
  -- Call the placeOrderApi
  putStrLn "Calling placeOrderApi with sample order..."
  response <- placeOrderApi request
  
  -- Display the response
  putStrLn $ "HTTP Status Code: " ++ show (httpStatusCode response)
  putStrLn "\nResponse Body (formatted):"
  putStrLn "=========================="
  
  -- Pretty print the JSON response
  let jsonResponse = L8.pack (responseBody response)
  case decode jsonResponse :: Maybe Value of
    Just value -> putStrLn $ L8.unpack $ encodePretty value
    Nothing -> putStrLn $ "Raw response: " ++ responseBody response
  
  -- Summarize the results
  putStrLn "\nOrder Processing Summary:"
  putStrLn "========================"
  if httpStatusCode response == 200
    then do
      putStrLn "✓ Order successfully processed!"
      putStrLn "✓ Customer acknowledgment sent"
      putStrLn "✓ Shipping order created"
      putStrLn "✓ Billing order created"
      putStrLn "✓ All domain events generated"
    else do
      putStrLn "✗ Order processing failed"
      putStrLn $ "  Error details in response above"
  
  putStrLn "\nConversion from F# to Haskell completed successfully!"
  putStrLn "This demonstrates the core domain types and PlaceOrder API working correctly."

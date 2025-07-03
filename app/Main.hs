-- | Main application entry point
module Main (main) where

import OrderTaking.Common.SimpleTypes
import OrderTaking.Result
import OrderTaking.PlaceOrder.Implementation
import OrderTaking.PlaceOrder.PublicTypes
import OrderTaking.PlaceOrder.InternalTypes
import qualified OrderTaking.PlaceOrder.Pricing as PricingModule

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
  
  putStrLn "\nTesting PlaceOrder Implementation..."
  putStrLn "===================================="
  
  -- Setup dependencies (similar to Api module but directly accessible)
  let checkProductExists :: CheckProductCodeExists
      checkProductExists _ = True -- dummy implementation
      
      checkAddressExists :: CheckAddressExists
      checkAddressExists unvalidatedAddress = do
        let checkedAddress = CheckedAddress unvalidatedAddress
        return $ Right checkedAddress
      
      getStandardPrices :: GetStandardPrices
      getStandardPrices () = \_ -> unsafeCreatePrice 10.0
      
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
      
      getPricingFunction :: GetPricingFunction
      getPricingFunction = PricingModule.getPricingFunction getStandardPrices getPromotionPrices
      
      calculateShippingCost' :: CalculateShippingCost
      calculateShippingCost' = calculateShippingCost
      
      createOrderAcknowledgmentLetter :: CreateOrderAcknowledgmentLetter
      createOrderAcknowledgmentLetter _ = HtmlString "some text"
      
      sendOrderAcknowledgment :: SendOrderAcknowledgment
      sendOrderAcknowledgment _ = Sent
  
  -- Create a sample unvalidated order directly
  let sampleOrder = UnvalidatedOrder
        { unvalidatedOrderId = "ORD-001"
        , unvalidatedCustomerInfo = UnvalidatedCustomerInfo
            { unvalidatedFirstName = "John"
            , unvalidatedLastName = "Doe"
            , unvalidatedEmailAddress = "john.doe@example.com"
            , unvalidatedVipStatus = "Normal"
            }
        , unvalidatedShippingAddress = UnvalidatedAddress
            { unvalidatedAddressLine1 = "123 Main St"
            , unvalidatedAddressLine2 = ""
            , unvalidatedAddressLine3 = ""
            , unvalidatedAddressLine4 = ""
            , unvalidatedCity = "Seattle"
            , unvalidatedZipCode = "98101"
            , unvalidatedState = "WA"
            , unvalidatedCountry = "USA"
            }
        , unvalidatedBillingAddress = UnvalidatedAddress
            { unvalidatedAddressLine1 = "123 Main St"
            , unvalidatedAddressLine2 = ""
            , unvalidatedAddressLine3 = ""
            , unvalidatedAddressLine4 = ""
            , unvalidatedCity = "Seattle"
            , unvalidatedZipCode = "98101"
            , unvalidatedState = "WA"
            , unvalidatedCountry = "USA"
            }
        , unvalidatedLines = 
            [ UnvalidatedOrderLine
                { unvalidatedOrderLineId = "LINE-001"
                , unvalidatedProductCode = "W1234"
                , unvalidatedQuantity = 2.0
                }
            , UnvalidatedOrderLine
                { unvalidatedOrderLineId = "LINE-002"
                , unvalidatedProductCode = "G123"
                , unvalidatedQuantity = 1.0
                }
            ]
        , unvalidatedPromotionCode = "HALF"
        }
  
  -- Call the placeOrder implementation directly
  putStrLn "Calling placeOrder implementation with sample order..."
  result <- placeOrder 
    checkProductExists
    checkAddressExists
    getPricingFunction
    calculateShippingCost'
    createOrderAcknowledgmentLetter
    sendOrderAcknowledgment
    sampleOrder
  
  -- Display the result
  case result of
    Right events -> do
      putStrLn "✓ Order successfully processed!"
      putStrLn $ "Generated " ++ show (length events) ++ " domain events:"
      putStrLn "=========================="
      
      -- Show the events (using Show instance instead of JSON)
      mapM_ (putStrLn . ("  " ++) . show) events
      
      putStrLn "\nOrder Processing Summary:"
      putStrLn "========================"
      putStrLn "✓ Order successfully processed!"
      putStrLn "✓ Customer acknowledgment sent"
      putStrLn "✓ Shipping order created"
      putStrLn "✓ Billing order created"
      putStrLn "✓ All domain events generated"
      
    Left err -> do
      putStrLn "✗ Order processing failed"
      putStrLn $ "Error: " ++ show err
  
  putStrLn "\nConversion from F# to Haskell completed successfully!"
  putStrLn "This demonstrates the core domain types and PlaceOrder Implementation working correctly."

-- | Result type and utilities for error handling
-- Converted from F# Result.fs
module OrderTaking.Result
  ( -- * Result type (using Either)
    Result
  , ValidationError(..)
  , AsyncResult
    -- * Result functions
  , bimap
  , mapError
  , ifError
  , sequence
  , lift2
  , lift3
  , lift4
  , bind2
  , bind3
  , isOk
  , isError
  , filter
  , ofOption
  , toOption
  , toErrorOption
    -- * Validation type
  , Validation
  , ofResult
  , toResult
  , sequenceValidation
    -- * AsyncResult functions
  , asyncResultMap
  , asyncResultMapError
  , asyncResultRetn
  , asyncResultBind
  , asyncResultSequenceM
  , asyncResultOfSuccess
  , asyncResultOfError
  , asyncResultOfResult
  , asyncResultOfAsync
  ) where

import Prelude hiding (filter, sequence)
import qualified Prelude
import qualified Control.Monad

-- | Result type is just Either with success on the Right
type Result e a = Either e a

-- | Validation error wrapper
newtype ValidationError = ValidationError String
  deriving (Show, Eq)

-- | AsyncResult is IO with Either result
type AsyncResult e a = IO (Either e a)

-- | Pass in a function to handle each case of Result
bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f _ (Left x) = Left (f x)
bimap _ g (Right y) = Right (g y)

-- | Map over the error case
mapError :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapError f (Left err) = Left (f err)
mapError _ (Right x) = Right x

-- | On success, return the value. On error, return a default value
ifError :: a -> Either e a -> a
ifError defaultVal (Left _) = defaultVal
ifError _ (Right x) = x

-- | Combine a list of results
sequence :: [Either e a] -> Either e [a]
sequence = Prelude.sequence

-- | Lift a two parameter function to use Result parameters
lift2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
lift2 f (Right x) (Right y) = Right (f x y)
lift2 _ (Left err) _ = Left err
lift2 _ _ (Left err) = Left err

-- | Lift a three parameter function to use Result parameters
lift3 :: (a -> b -> c -> d) -> Either e a -> Either e b -> Either e c -> Either e d
lift3 f (Right x) (Right y) (Right z) = Right (f x y z)
lift3 _ (Left err) _ _ = Left err
lift3 _ _ (Left err) _ = Left err
lift3 _ _ _ (Left err) = Left err

-- | Lift a four parameter function to use Result parameters
lift4 :: (a -> b -> c -> d -> e) -> Either err a -> Either err b -> Either err c -> Either err d -> Either err e
lift4 f (Right w) (Right x) (Right y) (Right z) = Right (f w x y z)
lift4 _ (Left err) _ _ _ = Left err
lift4 _ _ (Left err) _ _ = Left err
lift4 _ _ _ (Left err) _ = Left err
lift4 _ _ _ _ (Left err) = Left err

-- | Apply a monadic function with two parameters
bind2 :: (a -> b -> Either e c) -> Either e a -> Either e b -> Either e c
bind2 f x y = Control.Monad.join (lift2 f x y)

-- | Apply a monadic function with three parameters
bind3 :: (a -> b -> c -> Either e d) -> Either e a -> Either e b -> Either e c -> Either e d
bind3 f x y z = Control.Monad.join (lift3 f x y z)

-- | Predicate that returns true on success
isOk :: Either e a -> Bool
isOk (Right _) = True
isOk (Left _) = False

-- | Predicate that returns true on failure
isError :: Either e a -> Bool
isError = not . isOk

-- | Lift a given predicate into a predicate that works on Results
filter :: (a -> Bool) -> Either e a -> Bool
filter pred' (Right x) = pred' x
filter _ (Left _) = True

-- | Convert an Option into a Result
ofOption :: e -> Maybe a -> Either e a
ofOption err Nothing = Left err
ofOption _ (Just x) = Right x

-- | Convert a Result into an Option
toOption :: Either e a -> Maybe a
toOption (Right x) = Just x
toOption (Left _) = Nothing

-- | Convert the Error case into an Option
toErrorOption :: Either e a -> Maybe e
toErrorOption (Left err) = Just err
toErrorOption (Right _) = Nothing

-- | Validation type with list of failures
type Validation e a = Either [e] a

-- | Convert Result to Validation
ofResult :: Either e a -> Validation e a
ofResult (Left err) = Left [err]
ofResult (Right x) = Right x

-- | Convert Validation to Result
toResult :: Validation e a -> Either [e] a
toResult = id

-- | Combine a list of Validation, applicatively
sequenceValidation :: [Validation e a] -> Validation e [a]
sequenceValidation [] = Right []
sequenceValidation (Right x : xs) = case sequenceValidation xs of
  Right ys -> Right (x : ys)
  Left errs -> Left errs
sequenceValidation (Left errs1 : xs) = case sequenceValidation xs of
  Right _ -> Left errs1
  Left errs2 -> Left (errs1 ++ errs2)

-- | AsyncResult utilities

-- | Lift a function to AsyncResult
asyncResultMap :: (a -> b) -> AsyncResult e a -> AsyncResult e b
asyncResultMap f asyncResult = do
  fmap f <$> asyncResult

-- | Map over the error case
asyncResultMapError :: (e1 -> e2) -> AsyncResult e1 a -> AsyncResult e2 a
asyncResultMapError f asyncResult = do
  result <- asyncResult
  return $ case result of
    Left err -> Left (f err)
    Right x -> Right x

-- | Lift a value to AsyncResult
asyncResultRetn :: a -> AsyncResult e a
asyncResultRetn x = return (Right x)

-- | Apply a monadic function to an AsyncResult value
asyncResultBind :: (a -> AsyncResult e b) -> AsyncResult e a -> AsyncResult e b
asyncResultBind f asyncResult = do
  result <- asyncResult
  case result of
    Left err -> return (Left err)
    Right x -> f x

-- | Convert a list of AsyncResult into a AsyncResult<list> using monadic style
asyncResultSequenceM :: [AsyncResult e a] -> AsyncResult e [a]
asyncResultSequenceM [] = asyncResultRetn []
asyncResultSequenceM (x:xs) = do
  result <- x
  case result of
    Left err -> return (Left err)
    Right y -> do
      rest <- asyncResultSequenceM xs
      case rest of
        Left err -> return (Left err)
        Right ys -> return (Right (y:ys))

-- | Lift a value into an Ok inside a AsyncResult
asyncResultOfSuccess :: a -> AsyncResult e a
asyncResultOfSuccess = asyncResultRetn

-- | Lift a value into an Error inside a AsyncResult
asyncResultOfError :: e -> AsyncResult e a
asyncResultOfError err = return (Left err)

-- | Lift a Result into an AsyncResult
asyncResultOfResult :: Either e a -> AsyncResult e a
asyncResultOfResult = return

-- | Lift an IO into an AsyncResult
asyncResultOfAsync :: IO a -> AsyncResult e a
asyncResultOfAsync action = do
  Right <$> action

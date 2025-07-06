module OrderTaking.Undefined where

import Data.Void (Void)

-- a used for placeholders
type Undefined = Void

-- Will be replaced with ellipses in the book.
-- Used in the example code to let the code type check without having to provide full details.
type DotDotDot = Undefined 
type PrivateDotDotDot = Undefined 
dotDotDot = error "undefined"

-- Will be replaced with question marks in the book.
question = error "to be defined"

notImplemented = error "not implemented"

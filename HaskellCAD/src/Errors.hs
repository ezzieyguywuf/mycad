{-|
Module      : Errors
Description : Provides error-handling for the MyCAD application
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

The error handling is actually accomplished using the tried-and-true "Except"
monad.
-}
module Errors
(
-- * Data Types
  Error
, MyError(..)
-- * Exported Functions
, getErrorString
)where

-- | External imports
import Control.Monad.Except (Except)

type Error a = Except MyError a

-- | Indicates that there was an error in MyCAD
data MyError = EmptyInput
              | InvalidInput
              | UnknownAction
              | NumberParseError
              | PointParseError
              | NoRunnerAvailable
              deriving (Show)

-- | Returns a human-readable string for any given MyError
getErrorString :: MyError -> String
getErrorString err =
    case err of
        EmptyInput       -> emptyInput
        UnknownAction    -> unknownAction
        NumberParseError -> numberParseError
        PointParseError  -> pointParseError
        _                -> show err

emptyInput :: String
emptyInput = "Please provide some input, or [Ctrl-D] to exit"

unknownAction :: String
unknownAction = "That action is not known. Please use help for more information."

numberParseError :: String
numberParseError = "Expecting a string that can be interpreted as a Float. Must start with a Digit"

pointParseError :: String
pointParseError = "Expecting three consective Float, which will be (x,y,z) for a Point"

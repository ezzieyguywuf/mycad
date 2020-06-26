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
  ErrorT
, MyError(..)
-- * Exported Functions
, getErrorString
)where

-- | External imports
import Control.Monad.Except (ExceptT)

-- | Internal imports
import Entity (EntityState)

-- This error transformer allows to carry the EntityState while still managing
-- errors in the Except monad.
-- @p@ is the type of the "Geometry.Point" in the "EntityState"
-- @a@ is the return type
type ErrorT p a = ExceptT MyError (EntityState p) a

-- | Indicates that there was an error in MyCAD
data MyError = EmptyInput
              | InvalidInput
              | UnknownAction
              | FloatParseError
              | PointParseError
              | NoRunnerAvailable
              deriving (Show)

-- | Returns a human-readable string for any given MyError
getErrorString :: MyError -> String
getErrorString err =
    case err of
        EmptyInput      -> emptyInput
        UnknownAction   -> unknownAction
        FloatParseError -> floatParseError
        PointParseError -> pointParseError
        _               -> show err

emptyInput :: String
emptyInput = "Please provide some input, or [Ctrl-D] to exit"

unknownAction :: String
unknownAction = "That action is not known. Please use help for more information."

floatParseError :: String
floatParseError = "Expecting a string that can be interpreted as a Float. Must start with a Digit"

pointParseError :: String
pointParseError = "Expecting three consective Float, which will be (x,y,z) for a Point"

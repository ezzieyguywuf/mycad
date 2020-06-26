module Errors
(
  getErrorString
, ParseError (..)
)where

getErrorString :: ParseError -> String
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

-- ===========================================================================
--                      Private Free Functions and Stuff
-- ===========================================================================

-- | Indicates that there was an error parsing a user's input
data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownAction
                 | FloatParseError
                 | PointParseError
                   deriving (Show)


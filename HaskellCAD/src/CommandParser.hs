module CommandParser
(
  parseCommand
, filterKnown
)where

import qualified Data.Map as Map
import Data.List (isPrefixOf, uncons, intersperse)
import Commands

-- | Try to parse a String into a Command
parseCommand :: String -> Either String Runnable
parseCommand input = do
    (cmd, args) <- checkValid input
    matches     <- filterKnown' cmd
    case compare (length matches) 1 of
        EQ -> Right $ Runnable cmd' args
                  where Just cmd' = Map.lookup match knownCommands
                        [match]   = matches
        LT -> Left "checkValid function seems to have failed"
        GT -> Left $ "Perhaps you meant one of these: " <> (concat $ intersperse ", " matches)

-- | Returns a list of commands which partially match the provided String
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) (Map.keys knownCommands)

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
-- | Checks whether a user's input is valid or not
checkValid :: String -> Either String (String, [String])
checkValid s = case uncons (words s) of
                   Nothing -> Left "You must provide at least one character of input"
                   Just split -> Right split

-- | Either returns a list of commands with partially match the provided String or an error
filterKnown' :: String -> Either String [String]
filterKnown' s = case filterKnown s of
                    []  -> Left $ "The command \"" <> s
                                  <> "\" is not understood. Please type help for more help"
                    xs  -> Right xs

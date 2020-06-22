module Commands
(
  tryCommand
, filterKnown
, splitCommand
)where

import qualified Data.Map as Map
import Data.List (isPrefixOf, uncons)
import Entity

-- | Tries to execute the provided command on the given EntityState.
tryCommand :: String -> EntityState a String
tryCommand input = pure msg
    where msg = case splitCommand input of
                    Just (command, args) -> "I know about the command '" <> command <> "'!" 
                                            <> " Args = " <> (show args)
                    _ -> "Sorry, I'm not familiar with '" <> input <> "'"

-- | Returns a list of commands which partially match the provided String
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) (Map.keys knownCommands)

-- | Takes a String, and Maybe returns a tuple breking the String into (knownCommand, arguments)
splitCommand :: String -> Maybe (String, [String])
splitCommand info = do
    (maybeCommand, args) <- uncons $ words info
    case filterKnown maybeCommand of
        [command] -> Just (command, args)
        _    -> Nothing -- non-unique command prefix, or unknown command


-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
type Args = [String]

knownCommands :: Map.Map String (Args -> EntityState a ())
knownCommands = Map.fromList
    [
      ("add", doNothing)
    , ("delete", doNothing)
    , ("connect", doNothing)
    , ("construct", doNothing)
    ]
        where doNothing _ = pure () :: EntityState a ()

module Commands
(
  tryCommand
, filterKnown
, parseCommand
)where

import qualified Data.Map as Map
import Data.List (isPrefixOf, uncons, intersperse)
import Entity

-- | Tries to execute the provided command on the given EntityState.
tryCommand :: String -> EntityState a String
tryCommand input = case msg of
                       Nothing -> pure $ "Sorry, I'm not familiar with " <> input <> ""
                       Just s  -> s
    where msg = do (commandName, args) <- uncons (words input)
                   command <- Map.lookup commandName knownCommands
                   pure $ command args

-- | Returns a list of commands which partially match the provided String
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) (Map.keys knownCommands)

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
type Args = [String]

data Command = Add
             | Delete
             | Connect
             | Help
             deriving (Enum, Bounded)

parseCommand :: String -> Maybe (Command, Args)
parseCommand input = do
    (commandName, args) <- uncons (words input)
    case commandName of
        _ -> Just (Help, [])

--"These are the known commands: " <> (show commands)
                  --where commands :: [Command]
                        --commands = [(minBound :: Command) ..]

knownCommands :: Map.Map String (Args -> EntityState a String)
knownCommands = Map.fromList
    [
      ("add", doNothing)
    , ("delete", doNothing)
    , ("connect", doNothing)
    , ("help", help)
    ]
        where doNothing _ = pure "" :: EntityState a String

help :: Args -> EntityState a String
help _ = pure msg
    where msg = "Please type one of these commands: "
                <> concat (intersperse ", " (Map.keys knownCommands))

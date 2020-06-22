module Commands
(
  Command(..)
, Runnable(..)
, parseCommand
, execCommand
, filterKnown
)where

import qualified Data.Map as Map
import Data.List (isPrefixOf, uncons)
import Entity

-- | These are commands that can be executed upon an Entity
data Command = Add
             | Delete
             | Connect
             | Help
             deriving (Enum, Bounded, Show)

-- | This matches a Command with a list of one or more arguments to be run using "runCommand"
data Runnable = Runnable Command Args

-- | Try to parse a String into a Command
parseCommand :: String -> Either String Runnable
parseCommand input =
    case parsed of
        Nothing  -> Left $ "The command \"" <> input
                            <> "\" is not understood. Please type help for more help"
        Just (command, args) -> Right $ Runnable command args
    where parsed = do
          (commandName, args) <- uncons (words input)
          command <- Map.lookup commandName knownCommands
          pure (command, args)

-- | Either execute the command and return the mutated Entity, or return an error message
execCommand :: Runnable -> Either String (Entity a)
execCommand (Runnable Help _)    = Left $ "These are the known commands: " <> (show commands)
execCommand (Runnable command _) = Left $ "The command " <> (show command) <> " has not yet been implemented"


-- | Returns a list of commands which partially match the provided String
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) (Map.keys knownCommands)

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
type Args = [String]

commands :: [Command]
commands = [minBound ..]

knownCommands :: Map.Map String Command
knownCommands = Map.fromList
    [
      ("add", Add)
    , ("delete", Delete)
    , ("connect", Connect)
    , ("help", Help)
    ]

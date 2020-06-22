module Commands
(
  Command
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

-- | Try to parse a String into a Command
parseCommand :: String -> Maybe (Command, Args)
parseCommand input = do
    (commandName, args) <- uncons (words input)
    command <- Map.lookup commandName knownCommands
    pure (command, args)

-- | Either execute the command and return the mutated Entity, or return an error message
execCommand :: Command -> Either String (Entity a)
execCommand Help    = Left $ "These are the known commands: " <> (show commands)
execCommand command = Left $ "The command " <> (show command) <> " has not yet been implemented"


-- | Returns a list of commands which partially match the provided String
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) (Map.keys knownCommands)

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
type Args = [String]

commands :: [Command]
commands = [(minBound :: Command) ..]

knownCommands :: Map.Map String Command
knownCommands = Map.fromList
    [
      ("add", Add)
    , ("delete", Delete)
    , ("connect", Connect)
    , ("help", Help)
    ]

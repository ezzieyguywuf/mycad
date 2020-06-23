module Commands
(
  Command(..)
, Runnable(..)
, knownCommands
, commands
)where

import qualified Data.Map as Map

-- | These are commands that can be executed upon an Entity
data Command = Add
             | Delete
             | Connect
             | Help
             deriving (Enum, Bounded, Show)

-- | This matches a Command with a list of one or more arguments to be run using "runCommand"
data Runnable = Runnable Command Args

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

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
type Args = [String]


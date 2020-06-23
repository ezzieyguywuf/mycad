module CommandRunner
(
  Command (..)
, Runnable (..)
, runCommand
)
where

import Commands
import Entity

-- | Either execute the command and return the mutated Entity, or return an error message
runCommand :: Runnable -> Either String (Entity a)
runCommand (Runnable Help _)    = Left $ "These are the known commands: " <> (show commands)
runCommand (Runnable command _) = Left $ "The command " <> (show command) <> " has not yet been implemented"

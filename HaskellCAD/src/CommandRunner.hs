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
runCommand (Runnable Help [])    = Left $ runHelp ""
runCommand (Runnable Help args)    = Left $ runHelp (head args)
runCommand (Runnable Add args)  = runAdd args
runCommand (Runnable command _) = Left $ "The command " <> (show command) <> " has not yet been implemented"

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
runHelp :: String -> String
runHelp "add" = "Requires one of vertex or edge."
runHelp _ = "These are the known commands:\n"
            <> "    " <> (show commands) <> "\n"
            <> "Try help <command> for more information."

runAdd :: [String] -> Either String (Entity a)
runAdd [] = Left $ runHelp "add"

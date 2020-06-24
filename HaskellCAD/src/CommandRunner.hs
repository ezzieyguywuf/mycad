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
runCommand :: Entity a -> Runnable -> Either String (Entity a)
runCommand entity (Runnable Help [])      = Left $ runHelp ""
runCommand entity (Runnable Help (arg:_)) = Left $ runHelp arg
runCommand entity (Runnable Add (arg:_))  = runAdd entity arg
runCommand entity (Runnable command _)    =
    Left $ "The command " <> (show command) <> " has not yet been implemented"

-- ===========================================================================
--                   Private Free Functions and Data Type
-- ===========================================================================
runHelp :: String -> String
runHelp "add" = "Requires one of vertex or edge."
runHelp _ = "These are the known commands:\n"
            <> "    " <> (show commands) <> "\n"
            <> "Try help <command> for more information."

runAdd :: Entity a -> String -> Either String (Entity a)
runAdd _ "" = Left $ runHelp "add"
runAdd entity arg
  | arg == "vertex" = Left "Adding vertex"
  | arg == "edge"   = Left "Adding edge"
  | otherwise = Left $ runHelp "add"

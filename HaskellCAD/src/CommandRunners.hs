module CommandRunners
(
  runCommand
)where

import CommandParser (Command(..), Action(..))

runCommand :: Command -> String
runCommand cmd =
    case cmd of
        Help arg -> getHelpString arg
        _        -> "There is no runner for " <> show cmd

getHelpString :: Maybe Action -> String
getHelpString maction =
    case maction of
        Nothing     -> help
        Just action -> actionHelp action

help :: String
help = "This is Help"

actionHelp :: Action -> String
actionHelp action =
    case action of
        MakeVertex -> "This is addVertex help"
        _          -> help

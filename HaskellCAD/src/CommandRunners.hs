{-|
Module      : CommandRunner
Description : Used to execute a given Action in the MyCAD application
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

I'm not really sure if this is a sound design - I'm new enough to Haskell that
I don't really know all the different design patterns that are available or
typical

As it stands, the design is relatively simple:

    1. The "Except" monad is used to handle errors
    2. (coming soon) the "State" monad is used to manage the overall program's
       state
    3. A "String" is generated in order to provide feedback on the result of
       running the Action
-}
module CommandRunners
(
-- * Exported functions
  runCommand
)where

import CommandParser (Command(..), Action(..))
import Errors (Error)

-- | This will execute the "Command".
--
--   @Nothing@ indicates that the user has requested to exit the application
--   @Just s@ indicates that the "Command" executed succesfully - @s@ may be
--   empty or a message
--
--   Error-handling is done using the "Except" monad.
runCommand :: Command -> Error (Maybe String)
runCommand cmd =
    case cmd of
        Help arg -> pure . Just $ getHelpString arg
        Quit     -> pure Nothing
        _        -> pure . Just $ "There is no runner for " <> show cmd

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

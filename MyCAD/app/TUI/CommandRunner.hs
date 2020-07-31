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
module TUI.CommandRunner
(
-- * Exported functions
  runCommand
)where

-- Third-party
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (gets)

-- | Internal imports
import TUI.CommandParser ( Command(..), CommandToken(..), AddCommand(..))
import Entity (EntityState, addVertex, addEdge, prettyPrintEntity
              , vertexFromID)

-- | This will execute the "Command".
--
--   @Nothing@ indicates that the user has requested to exit the application
--   @Just s@ indicates that the "Command" executed succesfully - @s@ may be
--   empty or a message
--
--   Error-handling is done using the "Except" monad.
runCommand :: (Show p, Fractional p, Eq p) => Command p -> EntityState p (Maybe String)
runCommand cmd =
    case cmd of
        Help arg -> pure (Just $ getHelpString arg)
        Add acmd -> runAdd acmd
        Show     -> gets (Just . show . prettyPrintEntity)
        Quit     -> pure Nothing

runAdd :: (Fractional p, Eq p) => AddCommand p-> EntityState p (Maybe String)
runAdd cmd =
    case cmd of
        AddVertex point -> addVertex point >> pure (Just "Added a vertex")
        AddEdge n1 n2   -> runMaybeT $ do
            v1        <- MaybeT $ vertexFromID n1
            v2        <- MaybeT $ vertexFromID n2
            lift $ addEdge v1 v2 
            pure "Added a line"

getHelpString :: Maybe CommandToken -> String
getHelpString = maybe help commandHelp

help :: String
help = "This is Help"

commandHelp :: CommandToken -> String
commandHelp token =
    case token of
        AddT -> "This is add help"
        _    -> help

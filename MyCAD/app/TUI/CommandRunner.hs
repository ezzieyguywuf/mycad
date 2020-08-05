{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.State (gets)

-- | Internal imports
import TUI.CommandParser ( Command(..), CommandToken(..), AddCommand(..))
import Entity (EntityState, addVertex, addEdge, prettyPrintEntity
              , vertexFromID)

-- | This will execute the "Command".
--
--   Left msg       means that an error occured during execution
--   Right Nothing  means that he user has requested to exit the program
--   Right Just msg means that the command executed properly, and returned a
--                  (possibly empty) message
runCommand :: (Show p, Fractional p, Eq p)
           => Command p
           -> EntityState p (Either String (Maybe String))
runCommand cmd =
    case cmd of
        Help arg -> pure . Right . Just $ getHelpString arg
        Add acmd -> runAdd acmd
        Show     -> gets (Right . Just . show . prettyPrintEntity)
        Quit     -> pure (Right Nothing)

runAdd :: (Fractional p, Eq p)
       => AddCommand p
       -> EntityState p (Either String (Maybe String))
runAdd cmd =
    case cmd of
        AddVertex point -> addVertex point >> pure (Right . Just $ "Added a vertex")
        AddEdge n1 n2   -> runExceptT $ do
            v1 <- lift (vertexFromID n1) >>= note ("Can't find Vertex" <> show n1)
            v2 <- lift (vertexFromID n1) >>= note ("Can't find Vertex" <> show n2)
            lift (addEdge v1 v2)
            pure (Just "Added an Edge")

getHelpString :: Maybe CommandToken -> String
getHelpString = maybe help commandHelp

help :: String
help = "This is Help"

commandHelp :: CommandToken -> String
commandHelp token =
    case token of
        AddT -> "This is add help"
        _    -> help

note :: MonadError e m => e -> Maybe a -> m a
note msg = maybe (throwError msg) pure

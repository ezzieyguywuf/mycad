{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : LaunchTUI
Description : Launches MyCAD's Textual User Interface
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This will launch MyCAD's Textual User Interface. It provides a REPL-style
interface using the Haskell package Haskeline to achieve the CLI interface.

Haskeline itself seems pretty safe as a dependency - I've heard that even the
ubiquitous ghci using Haskeline behind-the-scene somewhere.

The Except monad (from mtl: again, rather ubiquitous) is used for error handling.
-}
module TUI.LaunchTUI (initialize, launch) where

-- | Base
import Data.Text (pack)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar)

-- | External imports
import Control.Monad.State  (runState)
import qualified System.Console.Haskeline as HL
import Text.Megaparsec.Error (errorBundlePretty)

-- | Internal imports
import TUI.CommandParser (ParseError, Command(Quit), parseInput, commandCompletions)
import TUI.CommandRunner (runCommand)
import Entity (Entity, nullEntity)

-- | Initializes the variables needed
initialize :: IO (TMVar (Entity p))
initialize = do
    putStrLn "Welcome to mycad. [Ctrl-d] or quit to exit."
    atomically $ newTMVar nullEntity

-- | Exit gracefully
exit :: HL.InputT IO ()
exit = HL.outputStrLn "exiting."

-- | Entry point for main loop
launch :: (Show p, Fractional p, Eq p) => TMVar (Entity p) -> IO ()
launch entityVar = HL.runInputT settings (loop entityVar)

-- | Determine if we should loop again or bail out.
loop :: (Show p, Fractional p, Eq p) => TMVar (Entity p) -> HL.InputT IO ()
loop entityVar = do
    -- First get the input from the user. Returns Nothing if ctrl-D was pressed
    userString <- HL.getInputLine "mycad> "
    let -- Turn a ctrl-D into a Quit command
        commandString = fromMaybe "quit" userString
        -- Parse the string into a Command
        eitherCommand = parseInput (pack commandString)

    -- Let handleCommand determine if we should loop again (note: handleError
    -- just prints out the error, then loops again)
    loopAgain <- either handleError (handleCommand entityVar) eitherCommand
    if loopAgain
       then loop entityVar
       else exit

-- | This will handle an error produced by the parser
handleError :: ParseError -> HL.InputT IO Bool
handleError pError =
    HL.outputStrLn (errorBundlePretty pError) >> pure True

-- | This will execute/handle a Command produced by the parser
handleCommand :: (Show a, Fractional a, Eq a)
              => TMVar (Entity a)
              -> Command a
              -> HL.InputT IO Bool
handleCommand _ Quit = pure False
handleCommand entityVar command = do
    liftIO $ do
        -- Get the current entity
        entity <- atomically (takeTMVar entityVar)
        -- run the command using the entity
        let (eitherRes, entity') = runState (runCommand command) entity
        case eitherRes of
            Left err -> putStrLn err
            Right maybeMsg ->
                -- Print out any messages that runCommand produced
                maybe (putStrLn "runCommand returned Nothing") putStrLn maybeMsg
        -- write back the (potentially) mutated entity
        atomically (putTMVar entityVar entity')
    pure True

-- ----------------------------------------------------------------------------
--                   Haskeline-Specific Setup Stuff. You can probably ignore
-- ----------------------------------------------------------------------------

-- | Provides setting information to InputT
settings :: HL.Settings IO
settings = 
    HL.Settings { HL.complete = HL.completeWord Nothing [' ', '\t'] completer
                , HL.historyFile = Nothing
                , HL.autoAddHistory = True
                }

-- | Provides tab-completion to Haskeline's InputT
completer :: String -> IO [HL.Completion]
completer s = pure $ map makeComplete (commandCompletions s)
    where makeComplete :: String -> HL.Completion
          makeComplete s' = HL.Completion s' s' False

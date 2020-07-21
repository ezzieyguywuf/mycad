{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar)

-- | External imports
import Control.Monad.State  (runState)
import qualified System.Console.Haskeline as HL
import Text.Megaparsec.Error (errorBundlePretty)

-- | Internal imports
import TUI.CommandParser (parseInput, commandCompletions)
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
launch :: (Show p, Fractional p) => TMVar (Entity p) -> IO ()
launch entityVar = HL.runInputT settings (loop entityVar)

-- | Determine if we should loop again or bail out.
loop :: (Show p, Fractional p) => TMVar (Entity p) -> HL.InputT IO ()
loop entityVar = do
    HL.getInputLine "mycad> " >>= \case
        Nothing    -> exit
        Just input -> case (parseInput (pack input)) of
            Left  err     -> do
                HL.outputStrLn (errorBundlePretty err)
                loop entityVar
            Right command -> do
                entity <- liftIO (atomically $ takeTMVar entityVar)
                case runState (runCommand entity command) entity of
                    (Nothing, _)        -> exit
                    (Just msg, entity') -> do
                        HL.outputStrLn msg
                        liftIO (atomically $ putTMVar entityVar entity')
                        loop entityVar

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

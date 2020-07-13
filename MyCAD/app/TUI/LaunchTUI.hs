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
module TUI.LaunchTUI (launch) where

-- | Base
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar)

-- | External imports
import Control.Monad.Except (runExcept)
import Control.Monad.State  (runState)
import qualified System.Console.Haskeline as HL

-- | Internal imports
import TUI.CommandParser (parseInput, commandCompletions)
import TUI.CommandRunner (runCommand)
import TUI.Errors (getErrorString)
import Entity (Entity, nullEntity)

-- | Entry point for program.
launch :: IO ()
launch = do
    putStrLn "Welcome to mycad. [Ctrl-d] to exit."
    entityVar <- atomically $ newTMVar (nullEntity :: Entity Float)
    HL.runInputT settings (mainLoop entityVar)

-- | Exit gracefully
exit :: HL.InputT IO ()
exit = HL.outputStrLn "exiting."

-- | Entry point for main loop
mainLoop :: (Show p, Fractional p) => TMVar (Entity p) -> HL.InputT IO ()
mainLoop entityVar = do
    input <- HL.getInputLine "mycad> "
    maybe exit (loopAgain entityVar) input

-- | Determine if we should loop again or bail out.
loopAgain :: (Show p, Fractional p) => TMVar (Entity p) -> String -> HL.InputT IO ()
loopAgain entityVar input =
    case runExcept (parseInput input) of
        Left  err     -> HL.outputStrLn (getErrorString err) >> mainLoop entityVar
        Right command -> do
            entity <- liftIO (atomically $ takeTMVar entityVar)
            case runState (runCommand entity command) entity of
                (Nothing, _)        -> exit
                (Just msg, entity') -> do
                    HL.outputStrLn msg
                    liftIO (atomically $ putTMVar entityVar entity')
                    mainLoop entityVar

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
          makeComplete s = HL.Completion s s False

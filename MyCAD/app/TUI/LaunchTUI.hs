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
    HL.runInputT settings (mainLoop (nullEntity :: Entity Float))

-- | Exit gracefully
exit :: HL.InputT IO ()
exit = HL.outputStrLn "exiting."

-- | Entry point for main loop
mainLoop :: (Show p, Fractional p) => Entity p -> HL.InputT IO ()
mainLoop entity = do
    input <- HL.getInputLine "mycad> "
    maybe exit (loopAgain entity) input

-- | Determine if we should loop again or bail out.
loopAgain :: (Show p, Fractional p) => Entity p -> String -> HL.InputT IO ()
loopAgain entity input =
    case runExcept (parseInput input) of
        Left  err     -> HL.outputStrLn (getErrorString err) >> mainLoop entity
        Right command -> case runState (runCommand entity command) entity of
                           (Nothing, _)        -> exit
                           (Just msg, entity') -> HL.outputStrLn msg >> mainLoop entity'

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

{-|
Module      : Main
Description : The main executable for MyCAD
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This is the main entry-point for the MyCAD application. It provides a
REPL-style interface using the Haskell package Haskeline to achieve the CLI
interface.

Haskeline itself seems pretty safe as a dependency - I've heard that even the
ubiquitous ghci using Haskeline behind-the-scene somewhere.

The Except monad (from mtl: again, rather ubiquitous) is used for error handling.
-}
module Main (main) where

-- | External imports
import Control.Monad.Except (runExceptT)
import Control.Monad.State  (runState)
import qualified System.Console.Haskeline as HL

-- | Internal imports
import CommandParser (parseInput, commandCompletions)
import CommandRunner (runCommand)
import Errors (getErrorString)
import Entity (Entity, nullEntity)

-- | Entry point for program.
main :: IO ()
main = do
    putStrLn "Welcome to mycad. [Ctrl-d] to exit."
    HL.runInputT settings (mainLoop nullEntity)

-- | Exit gracefully
exit :: HL.InputT IO ()
exit = HL.outputStrLn "exiting."

-- | Entry point for main loop
mainLoop :: Entity p -> HL.InputT IO ()
mainLoop entity = do
    input <- HL.getInputLine "mycad> "
    maybe exit (loopAgain entity) input

-- | Determine if we should loop again or bail out.
loopAgain :: Entity p -> String -> HL.InputT IO ()
loopAgain entity input =
    let estate    = runExceptT (parseInput input >>= runCommand entity)
        (check, entity') = runState estate entity
    in case check of
           Left  err        -> HL.outputStrLn (getErrorString err) >> mainLoop entity'
           Right (Just ret) -> HL.outputStrLn ret >> mainLoop entity'
           Right Nothing    -> exit

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

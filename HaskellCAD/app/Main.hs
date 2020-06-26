module Main (main) where

import Control.Monad.Except (runExcept)
import qualified System.Console.Haskeline as HL
import CommandParser (parseInput, commandCompletions)
import CommandRunners (runCommand)
import Errors (getErrorString)

-- | Entry point for program.
main :: IO ()
main = do
    putStrLn "Welcome to mycad. [Ctrl-d] to exit."
    HL.runInputT settings mainLoop

-- | Exit gracefully
exit :: HL.InputT IO ()
exit = HL.outputStrLn "exiting."

-- | Entry point for main loop
mainLoop :: HL.InputT IO ()
mainLoop = do
    input <- HL.getInputLine "mycad> "
    maybe exit loopAgain input

-- | Determine if we should loop again or bail out.
loopAgain :: String -> HL.InputT IO ()
loopAgain input =
    case runExcept (parseInput input >>= runCommand) of
        Left  err        -> HL.outputStrLn (getErrorString err) >> mainLoop
        Right (Just ret) -> HL.outputStrLn ret >> mainLoop
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

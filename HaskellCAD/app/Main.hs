module Main (main) where

import Control.Monad.Except (runExcept)
import System.Console.Haskeline
import CommandParser
import CommandRunners
import Errors

-- | Entry point for program.
main :: IO ()
main = do
    putStrLn "Welcome to mycad. [Ctrl-d] to exit."
    runInputT settings mainLoop

-- | Exit gracefully
exit :: InputT IO ()
exit = outputStrLn "exiting."

-- | Entry point for main loop
mainLoop :: InputT IO ()
mainLoop = do
    input <- getInputLine "mycad> "
    maybe exit loopAgain input

-- | Determine if we should loop again or bail out.
loopAgain :: String -> InputT IO ()
loopAgain input =
    case runExcept (parseInput input >>= runCommand) of
        Left  err        -> outputStrLn (getErrorString err) >> mainLoop
        Right (Just ret) -> outputStrLn ret >> mainLoop
        Right Nothing    -> exit

-- ----------------------------------------------------------------------------
--                   Haskeline-Specific Setup Stuff. You can probably ignore
-- ----------------------------------------------------------------------------

-- | Provides setting information to InputT
settings :: Settings IO
settings = Settings {
                      complete = completeWord Nothing [' ', '\t'] completer
                    , historyFile = Nothing
                    , autoAddHistory = True
                    }

-- | Provides tab-completion to Haskeline's InputT
completer :: String -> IO [Completion]
completer s = pure $ map makeComplete (commandCompletions s)
    where makeComplete :: String -> Completion
          makeComplete s = Completion s s False

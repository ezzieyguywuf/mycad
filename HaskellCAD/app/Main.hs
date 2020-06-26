module Main2 (main) where

import System.Console.Haskeline
import CommandParser

-- | Entry point for program.
main :: IO ()
main = do
    putStrLn "Welcome to mycad. [Ctrl-d] to exit."
    runInputT settings mainLoop

-- | Entry point for main loop
mainLoop :: InputT IO ()
mainLoop = do
    input <- getInputLine "mycad> "
    maybe (outputStrLn "exiting.") loopAgain input

-- | Determine if we should loop again or bail out.
loopAgain :: String -> InputT IO ()
loopAgain input =
    case parseInput input of
        Left err -> outputStrLn (show err)  >> mainLoop
        Right cmd -> outputStrLn (show cmd) >> mainLoop

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

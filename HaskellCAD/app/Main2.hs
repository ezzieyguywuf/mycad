module Main2 (main) where

import Data.List (isPrefixOf)

import System.Console.Haskeline
import CommandParser3

main :: IO ()
main = runInputT settings mainLoop

mainLoop :: InputT IO ()
mainLoop = do
    input <- getInputLine "mycad> "
    maybe (outputStrLn "exiting.") loopAgain input

loopAgain :: String -> InputT IO ()
loopAgain input =
    case parseInput input of
        Left err -> outputStrLn (show err)
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
completer s = pure $ map makeComplete commands
    where makeComplete :: String -> Completion
          makeComplete s = Completion s s False
          commands = filter (isPrefixOf s) knownCommands

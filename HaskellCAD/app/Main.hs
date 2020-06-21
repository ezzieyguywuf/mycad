import Data.List (isPrefixOf, uncons)
import System.Console.Haskeline

-- Evaluation : handle each line user inputs
cmd :: String -> InputT IO ()
cmd input = outputStrLn msg
    where msg = case splitCommand input of
                    Just (h, _) -> "I know about the command '" <> h <> "'!"
                    _ -> "Sorry, I'm not familiar with '" <> input <> "'"

splitCommand :: String -> Maybe (String, [String])
splitCommand s = do
    (h, t) <- uncons $ words s
    case filterKnown h of
        [h'] -> Just (h', t)
        _    -> Nothing -- non-unique command prefix

-- Tab Completion: return a completion for partial words entered
filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) names
    where names = ["add", "delete", "connect"]

completer :: String -> IO [Completion]
completer s = pure $ map makeComplete (filterKnown s)
    where makeComplete :: String -> Completion
          makeComplete s = Completion s s False

settings :: Settings IO
settings = Settings {
                      complete = completeWord Nothing [' ', '\t'] completer
                    , historyFile = Nothing
                    , autoAddHistory = True
                    }

main :: IO ()
main = runInputT settings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine ">>> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do cmd input
                                 loop

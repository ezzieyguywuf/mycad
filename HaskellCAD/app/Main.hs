import Data.List (isPrefixOf, uncons)
import System.Console.Haskeline
import Entity

-- Evaluation : handle each line user inputs
cmd :: String -> Entity a -> InputT IO (Entity a)
cmd input entity = outputStrLn msg >> pure entity
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

main :: IO (Entity a)
main = runInputT settings (loop nullEntity)

loop :: Entity a -> InputT IO (Entity a)
loop entity = do
    minput <- getInputLine ">>> "
    case minput of
        Nothing -> pure entity
        Just "quit" -> pure entity
        Just input -> cmd input entity >>= loop

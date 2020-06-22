import Data.List (isPrefixOf, uncons)
import System.Console.Haskeline
import Entity

-- Evaluation : handle each line user inputs
cmd :: String -> EntityState a () -> InputT IO (EntityState a ())
cmd input entity = outputStrLn msg >> pure entity
    where msg = case splitCommand input of
                    Just (command, args) -> "I know about the command '" <> command <> "'!" 
                                            <> " Args = " <> (show args)
                    _ -> "Sorry, I'm not familiar with '" <> input <> "'"

splitCommand :: String -> Maybe (String, [String])
splitCommand info = do
    (maybeCommand, args) <- uncons $ words info
    case filterKnown maybeCommand of
        [command] -> Just (command, args)
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

main :: IO (EntityState a ())
main = runInputT settings (loop emptyEntityState)

loop :: EntityState a () -> InputT IO (EntityState a ())
loop entity = do
    minput <- getInputLine ">>> "
    case minput of
        Nothing -> pure entity
        Just "quit" -> pure entity
        Just input -> cmd input entity >>= loop

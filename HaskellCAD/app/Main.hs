import System.Console.Haskeline
import Entity
import Commands

-- Evaluation : handle each line user inputs
evaluateCommand :: String -> EntityState a () -> InputT IO (EntityState a ())
evaluateCommand input entity = outputStrLn msg >> pure entity
    where msg = case splitCommand input of
                    Just (command, args) -> "I know about the command '" <> command <> "'!" 
                                            <> " Args = " <> (show args)
                    _ -> "Sorry, I'm not familiar with '" <> input <> "'"

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
        Just input -> evaluateCommand input entity >>= loop

import System.Console.Haskeline
import Entity
import Commands

-- | Run Haskeline
main :: IO ()
main = runInputT settings (loop nullEntity)

-- | Keep getting input from User until quit
loop :: Entity a -> InputT IO ()
loop entity = do
    minput <- getInputLine ">>> "
    case minput of
        Nothing -> pure ()
        Just "quit" -> pure ()
        Just input -> evaluateCommand input entity >>= loop

-- | Evaluation : handle each line user inputs
evaluateCommand :: String -> Entity a -> InputT IO (Entity a)
evaluateCommand input entity = do
    case parseCommand input of
        Nothing -> pure entity
        Just (command, _) -> case execCommand command of
                               Left msg      -> outputStrLn msg >> pure entity
                               Right entity' -> pure entity'

-- | Provides tab-completion to Haskeline's InputT
completer :: String -> IO [Completion]
completer s = pure $ map makeComplete (filterKnown s)
    where makeComplete :: String -> Completion
          makeComplete s = Completion s s False

-- | Provides setting information to InputT
settings :: Settings IO
settings = Settings {
                      complete = completeWord Nothing [' ', '\t'] completer
                    , historyFile = Nothing
                    , autoAddHistory = True
                    }

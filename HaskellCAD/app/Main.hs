import System.Console.Haskeline
import Entity
import CommandParser
import CommandRunner

-- | Run Haskeline
main :: IO ()
main = runInputT settings (loop nullEntity)

-- | Keep getting input from User until quit
loop :: Entity a -> InputT IO ()
loop entity = do
    minput <- getInputLine ">>> "
    maybe (pure ()) (parseInput entity) minput

parseInput :: Entity a -> String -> InputT IO ()
parseInput entity input =
    case input of
       "quit" -> pure ()
       input -> evaluateCommand input entity >>= loop

-- | Evaluation : handle each line user inputs
evaluateCommand :: String -> Entity a -> InputT IO (Entity a)
evaluateCommand input entity = do
    let output = parseCommand input >>= runCommand entity
    case output of
        Left msg -> outputStrLn msg >> pure entity
        Right (entity') -> pure entity'

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

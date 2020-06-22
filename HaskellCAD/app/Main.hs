import System.Console.Haskeline
import Entity
import Commands
import Control.Monad.State

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
    let (ret, entity') = runState (tryCommand input) entity
    outputStrLn ret
    pure entity'

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

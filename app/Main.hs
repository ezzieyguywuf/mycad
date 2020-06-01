import Data.List
import Control.Monad.IO.Class
import System.Console.Haskeline
import System.Console.Repline
import System.Process

type Repl a = HaskelineT IO a
type Command = (String, Repl ())

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
uniquePrefix :: String -> Maybe String
uniquePrefix s = out
    where vals = filterKnown s
          out | length vals == 1 = Just $ head vals
              | otherwise = Nothing

filterKnown :: String -> [String]
filterKnown s = filter (isPrefixOf s) names
    where names = ["add", "delete", "connect"]

completer :: Monad m => WordCompleter m
completer n = return $ filterKnown n

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to HaskellCAD, we hope your stay is pleasant."

main :: IO ()
main = evalRepl (pure ">>> ") cmd [] (Nothing) (Word completer) ini

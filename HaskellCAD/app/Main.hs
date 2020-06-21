import Data.List
import Control.Monad.IO.Class
import System.Console.Repline

type Repl a = HaskelineT IO a
type Command = (String, Repl ())

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print msg
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

completer :: Monad m => WordCompleter m
completer n = return $ filterKnown n

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to HaskellCAD, we hope your stay is pleasant."

main :: IO ()
main = evalRepl (pure ">>> ") cmd [] (Nothing) (Word completer) ini

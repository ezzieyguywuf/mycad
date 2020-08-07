{-# LANGUAGE OverloadedStrings #-}
module MyCADTest (main) where

--import Data.Text (Text)
import Control.Monad.State  (evalState)
import TUI.CommandParser (Command, parseInput)
import TUI.CommandRunner (runCommand)
import Entity (Entity, nullEntity, EntityState)
--import Text.Megaparsec.Error (errorBundlePretty)
import Data.Either (rights)

main :: IO ()
main = do
    let to_parse = [  "add vertex 0 0 0"
                   ,  "add vertex 10 20 30"
                   ,  "add vertex 5 30 10"
                   ,  "add line v0 v1"
                   ,  "add line v1 v2"
                   ,  "add line v2 v0"
                   ,  "show"
                   ,  "quit"
                   ]
        cmds = rights (fmap parseInput to_parse) :: [Command Float]
        states = fmap runCommand cmds
            :: [EntityState Float (Either String (Maybe String))]
        estate = sequence states
            :: EntityState Float [Either String (Maybe String)]
        entity = nullEntity :: Entity Float
        eitherStrings  = evalState estate entity  :: [Either String (Maybe String)]
        handleEither eith = case eith of
            Left msg -> putStrLn $ "Error: " <> msg
            Right mmsg -> case mmsg of
                Nothing -> putStrLn "quit requested"
                Just msg -> putStrLn msg
        ios = fmap handleEither eitherStrings
    putStrLn "Running the following commands: "
    mapM_ print cmds
    putStrLn "-------- Starting -------------"
    sequence_ ios
    putStrLn "-------- Done -------------"
--parseThings :: Text -> IO ()
--parseThings text =
    --case (parseInput text) of
        --Left err  -> putStrLn (errorBundlePretty err)
        --Right cmd -> do
            --let entity = nullEntity :: Entity Float
            --case runState (runCommand entity cmd) entity of
                --(Nothing, _)        -> putStrLn "I guess you want to quit?"
                --(Just msg, entity') -> do
                    --putStrLn msg
                    --putStrLn (show $ prettyPrintEntity entity')

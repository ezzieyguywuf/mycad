{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Data.Text (Text)
import Control.Monad.State  (evalState)
import TUI.CommandParser (Command, parseInput)
import TUI.CommandRunner (runCommand)
import Entity (Entity, nullEntity)
--import Text.Megaparsec.Error (errorBundlePretty)
import Data.Either (rights)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    let to_parse = [  "add vertex 10 20 30"
                   ,  "add vertex 40 50 60"
                   ,  "add vertex 70 80 90"
                   ,  "add vertex 60 70 80"
                   ,  "add vertex 50 60 70"
                   ,  "add line v0 v1"
                   ,  "add line v1 v2"
                   ,  "add line v2 v3"
                   ,  "add line v3 v4"
                   ,  "help help"
                   ,  "show"
                   ]
        cmds = rights (fmap parseInput to_parse) :: [Command Float]
        states = fmap runCommand cmds
        estate = sequence states
        entity = nullEntity :: Entity Float
        mStrings  = evalState estate entity  :: [Maybe String]
        ios = fmap putStrLn (catMaybes mStrings)
    putStrLn "Running the following commands: "
    sequence_ $ fmap (putStrLn . show) cmds
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

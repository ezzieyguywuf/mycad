{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Data.Text (Text)
--import Control.Monad.State  (runState)
import TUI.CommandParser (Command, parseInput)
--import TUI.CommandRunner (runCommand)
--import Entity (Entity, nullEntity, prettyPrintEntity)
--import Text.Megaparsec.Error (errorBundlePretty)
import Data.Either (rights)

main :: IO ()
main = do
    let to_parse = [  "add vertex 10 20 30"
                   ,  "add vertex 40 50 60"
                   ,  "add line v1 v2"
                   ,  "help help"
                   ,  "show"
                   ]
        cmds = rights (fmap parseInput to_parse) :: [Command Float]
        --let entity = nullEntity :: Entity Float
    sequence_ (fmap (putStrLn . show) cmds)

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

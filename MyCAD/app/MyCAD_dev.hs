{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import Control.Monad.State  (runState)
import TUI.CommandParser (parseInput)
import TUI.CommandRunner (runCommand)
import Entity (Entity, nullEntity, prettyPrintEntity)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    parseThings "add vertex 10 20 30"
    parseThings "add vertex 40 50 60"
    parseThings "add line v1 v2"
    parseThings "help help"

parseThings :: Text -> IO ()
parseThings text =
    case (parseInput text) of
        Left err  -> putStrLn (errorBundlePretty err)
        Right cmd -> do
            let entity = nullEntity :: Entity Float
            case runState (runCommand entity cmd) entity of
                (Nothing, _)        -> putStrLn "I guess you want to quit?"
                (Just msg, entity') -> do
                    putStrLn msg
                    putStrLn (show $ prettyPrintEntity entity')

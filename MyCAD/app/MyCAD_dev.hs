{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import Control.Monad.State  (runState)
import TUI.CommandParser (parseInput)
import TUI.CommandRunner (runCommand)
import Entity (nullEntity, prettyPrintEntity)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    parseThings "adpd vertex 10 20 30"
    parseThings "add vpertex 10 20 30"
    parseThings "add vertex 1a0 20 30"
    parseThings "add vertex 10 20 30"
    parseThings "help"
    parseThings "help ae"
    parseThings "help help"

parseThings :: Text -> IO ()
parseThings text =
    case  parseInput text of
        Left err  -> putStrLn (errorBundlePretty err)
        Right cmd -> do
            case runState (runCommand nullEntity cmd) nullEntity of
                (Nothing, _)        -> putStrLn "I guess you want to quit?"
                (Just msg, entity') -> do
                    putStrLn msg
                    putStrLn (show $ prettyPrintEntity entity')

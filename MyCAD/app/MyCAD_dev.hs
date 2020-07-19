{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import Control.Monad.State  (runState)
import TUI.CommandParser (parseInput)
import TUI.CommandRunner (runCommad)
import Entity (nullEntity)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    case parseInput "add vertex 10 20 30" of
        Left err  -> putStrLn (errorBundlePretty err)
        Right cmd -> do
            case runState (runCommand nullEntity cmd) nullEntity of
                (Nothing, _)        -> putStrLn "I guess you want to quit?"
                (Just msg, entity') -> do
                    putStrLn msg
                    putStrLn (show entity')

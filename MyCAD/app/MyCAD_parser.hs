{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import TUI.CommandParser2 (parseInput)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    runParser "help"
    runParser "help help"
    runParser "help help help"
    runParser "help quit"
    runParser "help show"
    runParser "quit"
    runParser "show"

runParser :: Text -> IO ()
runParser text =
    case parseInput text of
        Left err  -> putStrLn (errorBundlePretty err)
        Right cmd -> putStrLn . show $ cmd

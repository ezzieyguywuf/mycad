{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import TUI.CommandParser2 (parseInput)
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
    parseTest parseInput "help"
    parseTest parseInput "help help"
    parseTest parseInput "help help help"
    parseTest parseInput "help quit"
    parseTest parseInput "help show"
    parseTest parseInput "quit"
    parseTest parseInput "show"

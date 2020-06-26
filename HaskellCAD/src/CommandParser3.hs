module CommandParser3 where

import Data.List (uncons)

data ParserErrer = EmptyInput
                 | InvalidCommand
                   deriving (Show)

isStatement :: String -> Either ParseError [String]
isStatement input = case words input of
                       []    -> Left EmptyInput
                       split -> Right split

isCommand :: [String] -> Either ParserError (Command, [String])
isCommand input = case uncons input of
                      Nothing    -> Left InvalidCommand
                      Just split -> Right split

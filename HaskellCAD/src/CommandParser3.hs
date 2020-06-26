module CommandParser3 where

import Data.List (uncons)

data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownCommand
                   deriving (Show)

data Command = Help
               deriving (Show)

data Argument = String String
                deriving (Show)

isStatement :: String -> Either ParseError [String]
isStatement input =
    case words input of
       []    -> Left EmptyInput
       split -> Right split

isCommand :: [String] -> Either ParseError (Command, [String])
isCommand input =
    case uncons input of
       Nothing    -> Left InvalidInput
       Just (cmd,args) -> knownCommand cmd >>= \cmd' -> Right (cmd', args)

parseArgs :: (Command, [String]) -> Either ParseError (Command, [Argument])
parseArgs (cmd, args) =
    case cmd of
       Help -> []

knownCommand :: String -> Either ParseError Command
knownCommand string =
    case string of
       "help" -> Right Help
       _      -> Left UnknownCommand

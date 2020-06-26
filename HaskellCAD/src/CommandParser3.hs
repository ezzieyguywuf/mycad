module CommandParser3 where

import Data.List (uncons)

-- | Indicates that there was an error parsing a user's input
data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownAction
                   deriving (Show)

-- | 
data Action = Help
               deriving (Show)

data Argument = String String
                deriving (Show)

--parseInput :: String -> Either ParseError (Action, [Argument])

isStatement :: String -> Either ParseError [String]
isStatement input =
    case words input of
       []    -> Left EmptyInput
       split -> Right split

isAction :: [String] -> Either ParseError (Action, [String])
isAction input =
    case uncons input of
       Nothing    -> Left InvalidInput
       Just (cmd,args) -> knownAction cmd >>= \cmd' -> Right (cmd', args)

parseArgs :: (Action, [String]) -> Either ParseError (Action, [Argument])
parseArgs (cmd, args) =
    case cmd of
       Help -> []

knownAction :: String -> Either ParseError Action
knownAction string =
    case string of
       "help" -> Right Help
       _      -> Left UnknownAction

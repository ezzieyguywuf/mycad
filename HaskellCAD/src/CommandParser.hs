module CommandParser
(
  Command (..)
, parseInput
, commandCompletions
)where

import Data.List (uncons, isPrefixOf)

-- | A "Command" includes all the information necessary to execute an "Action"
data Command = Help (Maybe Action)
             | Quit
               deriving (Show, Eq)

-- | Takes a single "String", probably from IO, and returns a Command that can later be executed
parseInput :: String -> Either ParseError Command
parseInput string = isStatement string >>= isAction >>= hasArgs

commandCompletions :: String -> [String]
commandCompletions string = filter (isPrefixOf string) knownCommands
    where knownCommands = ["help"]

-- ===========================================================================
--                      Private Free Functions and Stuff
-- ===========================================================================

-- | Indicates that there was an error parsing a user's input
data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownAction
                   deriving (Show)

-- | An Action that can be performed
data Action = GetHelp
            | QuitProgram
              deriving (Show, Eq)

-- | These arguments may be passed to certion "Action"
data Argument = String String
                deriving (Show)

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

hasArgs :: (Action, [String]) -> Either ParseError Command
hasArgs (cmd, args) =
    case cmd of
       GetHelp     -> Right $ helpArgs args
       QuitProgram -> Right Quit

knownAction :: String -> Either ParseError Action
knownAction string =
    case string of
       "help" -> Right GetHelp
       "quit" -> Right QuitProgram
       _      -> Left UnknownAction

helpArgs :: [String] -> Command
helpArgs args = case isAction args of
                     Right (action, _) -> Help (Just action)
                     Left _ -> Help Nothing

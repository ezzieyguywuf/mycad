module CommandParser
(
  Command (..)
, parseInput
, commandCompletions
)where

import qualified Data.Map as Map

import Data.List (uncons, isPrefixOf)
import qualified Geometry as Geo

-- | A "Command" includes all the information necessary to execute an "Action"
data Command = Help (Maybe Action)
             | Quit
             | AddVertex (Geo.Point Float)
               deriving (Show, Eq)

-- | An Action that can be performed. This is not exported, it is just useful to place it here
data Action = GetHelp
            | QuitProgram
            | MakeVertex
              deriving (Show, Eq)

-- | This maps a string to each of our Action. This is not exported, but it's useful to have here.
actionMap :: Map.Map String Action
actionMap = Map.fromList
    [ ("help", GetHelp)
    , ("quit", QuitProgram)
    ]

-- | Takes a single "String", probably from IO, and returns a Command that can later be executed
parseInput :: String -> Either ParseError Command
parseInput string = isStatement string >>= isAction >>= hasArgs

-- | Provide a list of potential completions for the partial command supplied
commandCompletions :: String -> [String]
commandCompletions string = filter (isPrefixOf string) knownCommands
    where knownCommands = Map.keys actionMap

-- ===========================================================================
--                      Private Free Functions and Stuff
-- ===========================================================================

-- | Indicates that there was an error parsing a user's input
data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownAction
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
    case Map.lookup string actionMap of
       Just action -> Right action
       Nothing     -> Left UnknownAction

helpArgs :: [String] -> Command
helpArgs args = case isAction args of
                     Right (action, _) -> Help (Just action)
                     Left _ -> Help Nothing

module CommandParser
(
  Command (..)
, parseInput
, commandCompletions
)where

import qualified Data.Map as Map

import Data.List (uncons, isPrefixOf)
import qualified Geometry as Geo
import Data.Text (Text, pack, unpack, words)
import Data.Text.Read (rational)

type Point = Geo.Point Float

-- | A "Command" includes all the information necessary to execute an "Action"
data Command = Help (Maybe Action)
             | Quit
             | AddVertex Point
               deriving (Show, Eq)

-- | An Action that can be performed. This is not exported, it is just useful to place it here
data Action = GetHelp
            | QuitProgram
            | MakeVertex
              deriving (Show, Eq)

-- | This maps a string to each of our Action. This is not exported, but it's useful to have here.
actionMap :: Map.Map Text Action
actionMap = Map.fromList
    [ (pack "help", GetHelp)
    , (pack "quit", QuitProgram)
    , (pack "addVertex", MakeVertex)
    ]

-- | Takes a single "String", probably from IO, and returns a Command that can later be executed
parseInput :: String -> Either ParseError Command
parseInput string = parseStatement (pack string) >>= parseAction >>= parseCommand

-- | Provide a list of potential completions for the partial command supplied
commandCompletions :: String -> [String]
commandCompletions string = filter (isPrefixOf string) knownCommands
    where knownCommands = map unpack $ Map.keys actionMap

-- ===========================================================================
--                      Private Free Functions and Stuff
-- ===========================================================================

-- | Indicates that there was an error parsing a user's input
data ParseError = EmptyInput
                 | InvalidInput
                 | UnknownAction
                 | FloatParseError
                   deriving (Show)

-- ===========================================================================
--                                  Parsers
-- ===========================================================================
parseStatement :: Text -> Either ParseError [Text]
parseStatement input =
    case Data.Text.words input of
       []    -> Left EmptyInput
       split -> Right split

parseAction :: [Text] -> Either ParseError (Action, [Text])
parseAction input =
    case uncons input of
       Nothing    -> Left InvalidInput
       Just (cmd,args) -> case Map.lookup cmd actionMap of
                             Just action -> Right (action, args)
                             Nothing     -> Left UnknownAction

parseCommand :: (Action, [Text]) -> Either ParseError Command
parseCommand (cmd, args) =
    case cmd of
       GetHelp     -> Right $ parseHelpArgs args
       QuitProgram -> Right Quit
       MakeVertex  -> Right $ parseAddVertexArgs args

_parseFloat :: Text -> Either ParseError Float
_parseFloat text = do
    case rational text of
        Left _         -> Left FloatParseError
        Right (val, _) -> Right val

_parsePoint :: Text -> Either ParseError (Point)
_parsePoint text = undefined

-- ===========================================================================
--                           Argument  Parsers
-- ===========================================================================
parseHelpArgs :: [Text] -> Command
parseHelpArgs args = case parseAction args of
                     Right (action, _) -> Help (Just action)
                     Left _ -> Help Nothing

parseAddVertexArgs :: [Text] -> Command
parseAddVertexArgs args = undefined

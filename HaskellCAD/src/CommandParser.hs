module CommandParser
(
  Command (..)
, parseInput
, commandCompletions
)where

import Control.Monad.Except (Except, throwError, runExcept)
--import Control.Applicative ((<|>))
import qualified Data.Map as Map

import Data.List (uncons, isPrefixOf)
import qualified Geometry as Geo
import Data.Text (Text, pack, unpack, words, strip)
import Data.Text.Read (rational)
import Linear.V3

type Point = Geo.Point Float
type ParserError a = Except ParseError a

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
parseInput :: String -> ParserError Command
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
                 | PointParseError
                   deriving (Show)

-- ===========================================================================
--                                  Parsers
-- ===========================================================================
parseStatement :: Text -> ParserError [Text]
parseStatement input =
    case Data.Text.words input of
       []    -> throwError EmptyInput
       split -> pure split

parseAction :: [Text] -> ParserError (Action, [Text])
parseAction input =
    case uncons input of
       Nothing    -> throwError InvalidInput
       Just (cmd,args) -> case Map.lookup cmd actionMap of
                             Just action -> pure (action, args)
                             Nothing     -> throwError UnknownAction

parseCommand :: (Action, [Text]) -> ParserError Command
parseCommand (cmd, args) =
    case cmd of
       GetHelp     -> pure $ parseHelpArgs args
       QuitProgram -> pure Quit
       MakeVertex  -> pure $ parseAddVertexArgs args

parseFloat :: Text -> ParserError (Float, Text)
parseFloat text = do
    case rational (strip text) of
        Left _    -> throwError FloatParseError
        Right val -> pure val

_parsePoint :: Text -> ParserError (Point)
_parsePoint text = do
    (x, t0) <- parseFloat text
    (y, t1) <- parseFloat t0
    (z, t2) <- parseFloat t1
    pure (V3 x y z)
    -- <|> throwError PointParseError

-- ===========================================================================
--                           Argument  Parsers
-- ===========================================================================
parseHelpArgs :: [Text] -> Command
parseHelpArgs args = case runExcept (parseAction args) of
                     Right (action, _) -> Help (Just action)
                     Left _ -> Help Nothing

parseAddVertexArgs :: [Text] -> Command
parseAddVertexArgs args = undefined

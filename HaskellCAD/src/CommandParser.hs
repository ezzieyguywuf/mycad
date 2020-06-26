module CommandParser
(
  Command (..)
, Action  (..)
, parseInput
, commandCompletions
)where

import Control.Monad.Except (throwError, catchError)
import qualified Data.Map as Map

import Data.List (uncons, isPrefixOf)
import qualified Geometry as Geo
import Data.Text (Text, pack, unpack, words, strip, intercalate)
import Data.Text.Read (rational)
import Linear.V3

import Errors (Error, MyError(..))

type Point = Geo.Point Float

-- | A "Command" includes all the information necessary to execute an "Action"
data Command = Help (Maybe Action)
             | Quit
             | AddVertex Point
               deriving (Show, Eq)

-- | An Action that can be performed.
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
parseInput :: String -> Error Command
parseInput string = parseStatement (pack string) >>= parseAction >>= parseCommand

-- | Provide a list of potential completions for the partial command supplied
commandCompletions :: String -> [String]
commandCompletions string = filter (isPrefixOf string) knownCommands
    where knownCommands = map unpack $ Map.keys actionMap

-- ===========================================================================
--                                  Parsers
-- ===========================================================================
parseStatement :: Text -> Error [Text]
parseStatement input =
    case Data.Text.words input of
        []    -> throwError EmptyInput
        split -> pure split

parseAction :: [Text] -> Error (Action, [Text])
parseAction input =
    case uncons input of
        Nothing    -> throwError InvalidInput
        Just (cmd,args) -> case Map.lookup cmd actionMap of
                               Just action -> pure (action, args)
                               Nothing     -> throwError UnknownAction

parseCommand :: (Action, [Text]) -> Error Command
parseCommand (cmd, args) =
    case cmd of
        GetHelp     -> parseHelpArgs args
        QuitProgram -> pure Quit
        MakeVertex  -> parseAddVertexArgs args

parseFloat :: Text -> Error (Float, Text)
parseFloat text = do
    case rational (strip text) of
        Left _    -> throwError FloatParseError
        Right val -> pure val

parsePoint :: Text -> Error Point
parsePoint text = do
    (x, t0) <- parseFloat text
    (y, t1) <- parseFloat t0
    (z, t2) <- parseFloat t1
    pure (V3 x y z)
    `catchError` \_ -> (throwError PointParseError)

-- ===========================================================================
--                           Argument  Parsers
-- ===========================================================================
parseHelpArgs :: [Text] -> Error Command
parseHelpArgs args = do
    (action, _) <- parseAction args
    pure $ Help (Just action)
    `catchError` \_ -> (pure $ Help Nothing)

parseAddVertexArgs :: [Text] -> Error Command
parseAddVertexArgs args = do
    let args' = intercalate (pack " ") args
    point <- parsePoint args'
    pure $ AddVertex point
    `catchError` \_ -> (pure $ Help (Just MakeVertex))

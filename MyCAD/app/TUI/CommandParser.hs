{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : CommandParser
Description : Parses string commands to be used in the MyCAD application
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This module provides a string-parser which produces a "Command" to be used by
the MyCAD application.

This second attempt uses megaparsec for the parsing, rather than rolling our
own parser as we did at first
-}
module TUI.CommandParser
(
  Command(..)
, CommandToken(..)
, AddCommand(..)
, parseInput
, commandCompletions
)where

-- Base
import Data.Void (Void)
import Control.Applicative ((<|>), optional, empty, some)
import Data.Map (Map, keys, fromList, assocs)
import Data.List (isPrefixOf)

-- Third-Party
import Data.Text (Text, pack, unpack)
import Data.Text.Read (rational)
import Text.Megaparsec ((<?>), Parsec, parse, eof, choice, try)
import Text.Megaparsec.Char (string, space1, char, digitChar)
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Linear.V3 (V3(V3))

-- Internal
import Geometry (Point)

type Parser = Parsec Void Text
type ParseError = ParseErrorBundle Text Void

-- | A "Command" is an action that can be performed by the user.
--
--   This is parametrized over the type of Point to use
data Command a = Help (Maybe CommandToken)
               | Quit
               | Show
               | Add (AddCommand a)
                 deriving (Show)

-- | Tokenizes user's "Text" input into a recognizable \"add\" sub-command
data AddToken a = VertexToken
                | LineToken
                  deriving (Show)

data AddCommand a = AddVertex (Point a) deriving (Show)

-- | Tokenize a user's "Text" input into a recognizable commad.
data CommandToken  = HelpT
                   | QuitT
                   | ShowT
                   | AddT
                   deriving (Show)

-- | This will run our parser on the given line of input, generating a "Command"
parseInput :: Fractional a => Text -> Either ParseError (Command a)
parseInput = parse (spaceConsumer *> parseThings <* eof) ""
    where parseThings = lexeme lexCommand >>= parseCommand

-- | Parses the arguments for the given command
parseCommand :: Fractional a => CommandToken -> Parser (Command a)
parseCommand token =
    case token of
        HelpT -> parseHelp
        QuitT -> pure Quit
        ShowT -> pure Show
        AddT  -> lexeme lexAdd >>= parseAdd

-- | Given some string, determines if this partially matches any of our known "Command"
--
--   There's an opportunity to allow for fuzzy matching, but that's not
--   currently implemented.
commandCompletions :: String -> [String]
commandCompletions text = filter (isPrefixOf text) commands
    where commands = map unpack $ keys knownCommands

-------------------------------------------------------------------------------
--                      Internal stuff
-------------------------------------------------------------------------------
knownCommands :: Map Text CommandToken
knownCommands = fromList
    [ ("help", HelpT)
    , ("quit", QuitT)
    , ("show", ShowT)
    , ("add" , AddT)
    ]

-- | This is a list of the sub-commands recognized by the add "Command"
addCommands :: Map Text (AddToken a)
addCommands = fromList
    [ ("vertex", VertexToken)
    , ("line"  , LineToken)]

-- | Tries to parse a single "CommandToken"
lexCommand :: Parser CommandToken
lexCommand = checkMap knownCommands <?> "known command. Check help"

-- | Tries to parse a single "AddToken"
lexAdd :: Parser (AddToken a)
lexAdd = checkMap addCommands <?> "known \"add\" sub-command. Check \"help add\""

-- | Tries to parse the given \"add\" sub-command's arguments
parseAdd :: (Fractional a) => AddToken a -> Parser (Command a)
parseAdd token =
    case token of
        VertexToken -> parsePoint >>= pure . Add . AddVertex
        LineToken   -> undefined

-- | Takes a "Map Text a" as input, and tries to parse each key. On success,
--   returns the coresponding value
checkMap :: Map Text a -> Parser a
checkMap m = choice (fmap check (assocs m))
    where check (key, a) = string key >> pure a

-- | This parses any arguments to the \"help\" command
parseHelp :: Fractional a => Parser (Command a)
parseHelp = optional (lexeme lexCommand) >>= pure . Help

-- | This parses a 3-dimensional point x y z
parsePoint :: Fractional a => Parser (Point a)
parsePoint = do
    x <- lexeme number
    y <- lexeme number
    z <- lexeme number
    pure (V3 x y z)

-- | This will....consume space
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

-- | Use the given parser to parse a lexeme, consuming any space after
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Use to parse an integer
integer :: Parser String
integer = some digitChar

-- | A float with a trailing decimal
--
--   We add a zero after the decimal, in order to make parsing it easier
floatTrailing :: Parser String
floatTrailing = do
    val <- integer
    char '.'
    pure $ val <> ".0"

-- | A float with a leading decimal
--
--   We add a zero before the decimal, in order to make parsing it easier
floatLeading :: Parser String
floatLeading = do
    char '.'
    val <- integer
    pure $ "0." <> val

-- | A float with numbers both before and after the decimal
floatBoth :: Parser String
floatBoth = do
    before <- integer
    char '.'
    after <- integer
    pure $ before <> "." <> after

-- | Use to parse a float
--
--   It can be input as:
--      1. a standalone integer, e.g. "1234"
--      2. a number with a trailing decimal, e.g. "1234."
--      3. a number with a leading decimal, e.g. ".1234"
--      4. an arbitrary decimal value "12.34"
float :: Parser String
float = try floatLeading
    <|> try floatTrailing
    <|> try floatBoth
    <|> integer

-- | Use to parse a number
number :: Fractional a => Parser a
number = do
    text <- float
    case rational . pack $ text of
        Left err       -> fail err
        Right (val, _) -> pure val

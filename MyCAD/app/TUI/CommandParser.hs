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
, Item(..)
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
               | Add (Item a)
                 deriving (Show)

-- | Something that can be added
data Item a = VertexItem (Point a) deriving (Show)

-- | Tokenize a user's "Text" input into a recognizable commad.
data CommandToken  = HelpToken
                   | QuitToken
                   | ShowToken
                   | AddToken
                   deriving (Show)

-- | This will run our parser on the given line of input, generating a "Command"
parseInput :: Fractional a => Text -> Either ParseError (Command a)
parseInput = parse (spaceConsumer *> parseThings <* eof) ""
    where parseThings = lexeme (parseCommand <?> "valid command") >>= parseArgs

-- | This will parse an abritrary line of input from the User.
--
--   Note that this will only parse a single line, which must issue some
--   "Command"
parseCommand :: Parser CommandToken
parseCommand = lexeme lexCommand

-- | Parses the arguments for the given command
parseArgs :: Fractional a => CommandToken -> Parser (Command a)
parseArgs token =
    case token of
        HelpToken -> parseHelpArgs
        QuitToken -> pure Quit
        ShowToken -> pure Show
        AddToken  -> parseAddArgs

-- | Given some string, determines if this partially matches any of our known "Command"
--
--   There's an opportunity to allow for fuzzy matching, but that's not
--   currently implemented.
commandCompletions :: String -> [String]
commandCompletions string = filter (isPrefixOf string) commands
    where commands = map unpack $ keys knownCommands

-------------------------------------------------------------------------------
--                      Internal stuff
-------------------------------------------------------------------------------
knownCommands :: Map Text CommandToken
knownCommands = fromList
    [ ("help", HelpToken)
    , ("quit", QuitToken)
    , ("show", ShowToken)
    , ("add" , AddToken)
    ]

-- | Tries to parse a single "CommandToken"
lexCommand :: Parser CommandToken
lexCommand = choice (fmap check (assocs knownCommands))
    where check (key, token) = string key >> pure token

-- | This parses any arguments to the \"help\" command
parseHelpArgs :: Fractional a => Parser (Command a)
parseHelpArgs =
    try $ do string "help"
             pure (Help (Just HelpToken))
    <|> (optional (lexeme parseCommand) >>= pure . Help)

-- | This parses any arguments to the \"add\" command
parseAddArgs :: Fractional a => Parser (Command a)
parseAddArgs = do
    lexeme $ string "vertex"
    point <- parsePoint
    pure $ Add (VertexItem point)

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

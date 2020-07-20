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
, parseInput
)where

-- Base
import Data.Void (Void)
import Control.Applicative ((<|>), optional, empty, some)
import Data.Map (Map, fromList, assocs)

-- Third-Party
import Data.Text (Text, pack)
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
data Command a = Help (Maybe (Command a))
               | Quit
               | Show
               | Add (Item a)
                 deriving (Show)

-- | Something that can be added
data Item a = VertexItem (Point a) deriving (Show)

-- | This will run our parser on the given input, generating a "Command"
parseInput :: Fractional a => Text -> Either ParseError (Command a)
parseInput = parse startParsing ""

-- | This will parse an abritrary line of input from the User.
--
--   Note that this will only parse a single line, which must issue some
--   "Command"
startParsing :: Fractional a => Parser (Command a)
startParsing = spaceConsumer *> lexeme parseCommand <* eof

-------------------------------------------------------------------------------
--                      Internal stuff
-------------------------------------------------------------------------------
knownCommands :: Fractional a => Map Text (Parser (Command a))
knownCommands = fromList
    [ ("help", parseHelpArgs)
    , ("quit", pure Quit)
    , ("show", pure Show)
    , ("add" , parseAddArgs)
    ]

-- | Try to parse each key in the "knownCommands" "Data.Map"
--
--   If the parse succeeds, it will return the accompanying "Command" in the
--   value associated with said key.
parseCommand :: Fractional a => Parser (Command a)
parseCommand =
        choice (fmap checkCommand (assocs knownCommands))
    <?> "valid command"

-- | This parses any arguments to the \"help\" command
parseHelpArgs :: Fractional a => Parser (Command a)
parseHelpArgs =
    try
        (do word "help"
            pure (Help (Just (Help Nothing))))
    <|> do cmd <- optional parseCommand
           pure (Help cmd)

-- | This parses any arguments to the \"add\" command
parseAddArgs :: Fractional a => Parser (Command a)
parseAddArgs = do
    word "vertex"
    point <- parsePoint
    pure $ Add (VertexItem point)

-- | This parses a 3-dimensional point x y z
parsePoint :: Fractional a => Parser (Point a)
parsePoint = do
    x <- number
    y <- number
    z <- number
    pure (V3 x y z)

-- | Tries to parse (String, Parser a) pair
checkCommand :: (Text, Parser (Command a)) -> Parser (Command a)
checkCommand (text, parser) = word text >> parser

-- | This will....consume space
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

-- | Use the given parser to parse a lexeme, consuming any space after
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

-- | Use to parse a single word
word :: Text -> Parser Text
word = lexeme . string

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
number = lexeme $ do
    text <- float
    case rational . pack $ text of
        Left err       -> fail err
        Right (val, _) -> pure val

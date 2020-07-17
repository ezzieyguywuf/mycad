{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : CommandParser2
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
module TUI.CommandParser2
(
  parseInput
)where

-- Base
import Data.Void (Void)
import Control.Applicative ((<|>), optional, empty)
import Data.Map (Map, fromList, assocs)

-- Third-Party
import Data.Text (Text)
import Text.Megaparsec ((<?>), Parsec, parse, eof, choice, try)
import Text.Megaparsec.Char (string, space1)
import Text.Megaparsec.Error (ParseErrorBundle)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text
type ParseError = ParseErrorBundle Text Void

-- | A "Command" is an action that can be performed by the user.
data Command = Help (Maybe Command)
             | Quit
             | Show
               deriving (Show, Eq)

-- | This will run our parser on the given input, generating a "Command"
parseInput :: Text -> Either ParseError Command
parseInput = parse startParsing ""

-- | This will parse an abritrary line of input from the User.
--
--   Note that this will only parse a single line, which must issue some
--   "Command"
startParsing :: Parser Command
startParsing = lexeme parseCommand <* eof

-------------------------------------------------------------------------------
--                      Internal stuff
-------------------------------------------------------------------------------
knownCommands :: Map Text (Parser Command)
knownCommands = fromList
    [ ("help", parseHelpArgs)
    , ("quit", pure Quit)
    , ("show", pure Show)
    ]

-- | Try to parse each key in the "knownCommands" "Data.Map"
--
--   If the parse succeeds, it will return the accompanying "Command" in the
--   value associated with said key.
parseCommand :: Parser Command
parseCommand =
        choice (fmap checkCommand (assocs knownCommands))
    <?> "valid command"

-- | This parses any arguments to the \"help\" command
parseHelpArgs :: Parser Command
parseHelpArgs =
    try
        (do word "help"
            pure (Help (Just (Help Nothing))))
    <|> do cmd <- optional parseCommand
           pure (Help cmd)


-- | Tries to parse (String, Parser a) pair
checkCommand :: (Text, Parser Command) -> Parser Command
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

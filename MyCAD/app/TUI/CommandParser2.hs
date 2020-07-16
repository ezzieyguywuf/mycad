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
import Text.Megaparsec ((<?>), Parsec, eof, choice)
import Text.Megaparsec.Char (string, space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

-- | A "Command" is an action that can be performed by the user.
data Command = Help (Maybe Command)
             | Quit
             | Show
               deriving (Show, Eq)

parseInput :: Parser Command
parseInput =
        lexeme parseCommand <* eof
    <|> lexeme parseHelp <* eof

parseCommand :: Parser Command
parseCommand =
        choice (fmap checkCommand (assocs knownCommands))
    <?> "valid command"

parseHelp :: Parser Command
parseHelp = do
    cmd <- optional parseCommand
    pure (Help cmd)

-------------------------------------------------------------------------------
--                      Internal stuff
-------------------------------------------------------------------------------
knownCommands :: Map Text (Parser Command)
knownCommands = fromList
    [ ("help", parseHelp)
    , ("quit", pure Quit)
    , ("show", pure Show)
    ]

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

{-# LANGUAGE OverloadedStrings #-}
module CommandParser2 where

import qualified Geometry as Geo
import Data.Char (isDigit)

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text
import Data.Void

import Linear.V3

type Parser = Parsec Void Text

-- | This is a command that the user can request
data Command a = Add (Item a)
               | Help
               deriving (Show)

-- | This is something that the user can Add
data Item a = Vertex (Geo.Point a)
              deriving (Show)

parseCommand :: Parser Text
parseCommand = choice [ string "add"
                      , string "help"
                      ]

parseInteger :: Parser Int
parseInteger = some digitChar >>= pure . read

maybeInteger :: Parser (Maybe Int)
maybeInteger = do
    (some digitChar >>= pure . Just . read)
    <|> pure Nothing

--parsePoint' :: Parser (Geo.Point Int)
--parsePoint' = do
    --x <- parseInteger
    --space
    --y <- parseInteger
    --space
    --z <- parseInteger
    --pure $ V3 x y z

--maybePoint :: ReadP (Maybe (Geo.Point Int))
--maybePoint = do
    --x <- parseInteger <++ (pure Nothing)
    --y <- parseInteger <++ (pure Nothing)
    --z <- parseInteger <++ (pure Nothing)
    --pure $ Just (V3 x y z)

maybeCommand :: Num a => Parser (Maybe (Command a))
maybeCommand = do
    parsed <- parseCommand
    case parsed of
       ""     -> pure $ Nothing
       "help" -> pure $ Just Help
       "add"  -> pure $ parsePoint >>= (\x -> Just (Add x))
       _      -> pure $ Nothing

parsePoint :: Num a => (Maybe (Item a))
parsePoint = Just (Vertex (V3 10 10 10))

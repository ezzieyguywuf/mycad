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

maybeCommand :: Parser (Maybe Text)
maybeCommand = do
    fmap Just $ choice [ string "add"
                       , string "help"
                       ]
    <|> pure Nothing

maybeInteger :: Parser (Maybe Int)
maybeInteger = do
    (fmap (Just . read) (some digitChar))
    <|> pure Nothing

maybePoint :: Parser (Maybe (Geo.Point Int))
maybePoint = do
    x <- maybeInteger
    space
    y <- maybeInteger
    space
    z <- maybeInteger
    pure $ V3 <$> x <*> y <*> z

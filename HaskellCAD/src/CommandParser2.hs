module CommandParser2 where

import qualified Geometry as Geo
import Data.Char (isDigit)

import Text.ParserCombinators.ReadP
import Linear.V3

-- | This is a command that the user can request
data Command a = Add (Item a)
               | Help
               deriving (Show)

-- | This is something that the user can Add
data Item a = Vertex (Geo.Point a)
              deriving (Show)

parseCommand :: ReadP String
parseCommand = choice [ string "add"
                      , string "help"
                      ]

parseInteger :: ReadP Int
parseInteger = do
    x <- munch1 isDigit
    pure $ read x

parsePoint' :: ReadP (Geo.Point Int)
parsePoint' = do
    x <- parseInteger
    skipSpaces
    y <- parseInteger
    skipSpaces
    z <- parseInteger
    pure $ V3 x y z

--maybePoint :: ReadP (Maybe (Geo.Point Int))
--maybePoint = do
    --val <- parsePoint'
    --case val of
        --[]    -> pure Nothing
        --point -> pure $ Just point

maybeCommand :: Num a => ReadP (Maybe (Command a))
maybeCommand = do
    parsed <- parseCommand
    case parsed of
       ""     -> pure $ Nothing
       "help" -> pure $ Just Help
       "add"  -> pure $ parsePoint >>= (\x -> Just (Add x))
       _      -> pure $ Nothing

parsePoint :: Num a => (Maybe (Item a))
parsePoint = Just (Vertex (V3 10 10 10))

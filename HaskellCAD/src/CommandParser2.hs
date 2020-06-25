module CommandParser2 where

import qualified Geometry as Geo

import Text.ParserCombinators.ReadP

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

maybeCommand :: ReadP (Maybe (Command a))
maybeCommand = do
    parsed <- parseCommand
    case parsed of
       ""     -> pure $ Nothing
       "help" -> pure $ Just Help
       --"add"  -> parsePoint >>= (Just Add)
       _      -> pure $ Nothing

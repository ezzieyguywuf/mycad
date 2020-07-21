module Main (main) where

import Control.Concurrent.STM.TMVar (TMVar)
import TUI.LaunchTUI (initialize, launch)
import Entity (Entity)

main :: IO ()
main = do putStrLn "Hello, MyCAD!"
          entity <- initialize :: IO (TMVar (Entity Float))
          launch entity

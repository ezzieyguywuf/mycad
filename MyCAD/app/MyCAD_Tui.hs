module Main (main) where

import TUI.LaunchTUI (initialize, launch)

main :: IO ()
main = do putStrLn "Hello, MyCAD!"
          initialize >>= launch

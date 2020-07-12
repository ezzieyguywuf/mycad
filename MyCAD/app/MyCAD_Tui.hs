module Main (main) where

import TUI.LaunchTUI (launch)

main :: IO ()
main = do putStrLn "Hello, MyCAD!"
          launch

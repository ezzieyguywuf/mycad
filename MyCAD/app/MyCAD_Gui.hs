module Main (main) where

import GUI.LaunchGUI (launch)

main :: IO ()
main = do putStrLn "Hello, MyCAD, GUI edition!"
          launch

module Main (main) where

import GUI.LaunchGUI (initialize, launch)

main :: IO ()
main = do putStrLn "Hello, MyCAD, GUI edition!"
          initialize >>= launch

module Main (main) where

-- Base
import Control.Concurrent (forkIO)

-- Internal
import qualified TUI.LaunchTUI as TUI
import qualified GUI.LaunchGUI as GUI

main :: IO ()
main = do entity <- TUI.initialize
          queue  <- GUI.initialize

          forkIO (TUI.launch entity)
          GUI.launch queue

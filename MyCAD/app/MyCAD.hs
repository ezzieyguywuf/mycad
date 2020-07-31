module Main (main) where

-- Base
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar (TMVar)

-- Internal
import qualified TUI.LaunchTUI as TUI
import qualified GUI.LaunchGUI as GUI
import Entity (Entity)

main :: IO ()
main = do entity <- TUI.initialize :: IO (TMVar (Entity Float))
          queue  <- GUI.initialize

          forkIO (runTUI entity)
          GUI.launch queue

          -- TODO: Use GUI.RenderQueue.queueObject with queue well as
          -- GUI.GL.Primitives.makeLine to actually render whatever we have in
          -- our Entity.
          --
          -- We'll have to somehow keep track of when our entity changes, I
          -- guess in he forked thread?
          --
          -- TODO: Catch quit signal from TUI and shut down GUI.

runTUI :: (Show a, Fractional a, Eq a) => TMVar (Entity a) -> IO ()
runTUI = TUI.launch

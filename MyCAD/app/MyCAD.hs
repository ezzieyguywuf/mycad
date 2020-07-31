module Main (main) where

-- Base
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TMVar (TMVar)

-- Internal
import qualified TUI.LaunchTUI as TUI
import qualified GUI.LaunchGUI as GUI
import GUI.RenderQueue (RenderQueue)
import Entity (Entity)

main :: IO ()
main = do entity <- TUI.initialize :: IO (TMVar (Entity Float))
          queue  <- GUI.initialize

          forkIO (runTUI entity)
          forkIO (renderEntity entity queue)
          GUI.launch queue

          -- TODO: Use GUI.RenderQueue.queueObject with queue well as
          -- GUI.GL.Primitives.makeLine to actually render whatever we have in
          -- our Entity.
          --
          -- We'll have to somehow keep track of when our entity changes, I
          -- guess in he forked thread?
          --
          -- TODO: Catch quit signal from TUI and shut down GUI.

runTUI :: (Show a, Fractional a, Eq a)
       => TMVar (Entity a)
       -> IO ()
runTUI = TUI.launch

renderEntity :: (Show a, Fractional a, Eq a)
            => TMVar (Entity a)
            -> RenderQueue
            -> IO ()
renderEntity entityVar queue = do
    threadDelay 100
    renderEntity entityVar queue

module Main (main) where

-- Base
import Control.Monad (unless)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, readTMVar)

-- Internal
import qualified TUI.LaunchTUI as TUI
import qualified GUI.LaunchGUI as GUI
import GUI.RenderQueue (RenderQueue)
import Entity (Entity)

main :: IO ()
main = do entity <- TUI.initialize :: IO (TMVar (Entity Float))
          queue  <- GUI.initialize

          -- Run the TUI in a separate thread
          forkIO (runTUI entity)

          -- renderEntity compares the Entity passed in to the current value in
          -- the TMVar to determine if it needs to render. Therefore, we need
          -- to grab an initial value to pass
          initialEntity <- atomically (readTMVar entity)
          forkIO (renderEntity initialEntity entity queue)
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
            => Entity a
            -> TMVar (Entity a)
            -> RenderQueue
            -> IO ()
renderEntity lastEntity entityVar queue = do
    entity <- atomically (readTMVar entityVar)
    unless (entity == lastEntity) (putStrLn "Something changed!")
    threadDelay 100
    renderEntity entity entityVar queue

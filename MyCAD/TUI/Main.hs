{-|
Module      : TuiMain
Description : The main executable for MyCAD
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This is the main entry-point for the MyCAD TUI application.
-}
module Main (main) where

import LaunchTUI (launch)

main :: IO ()
main = launch

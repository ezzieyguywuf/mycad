{-|
Module      : TuiMain
Description : The main executable for MyCAD
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

This is the main entry-point for the MyCAD application. It provides a
REPL-style interface using the Haskell package Haskeline to achieve the CLI
interface.

Haskeline itself seems pretty safe as a dependency - I've heard that even the
ubiquitous ghci using Haskeline behind-the-scene somewhere.

The Except monad (from mtl: again, rather ubiquitous) is used for error handling.
-}
module Main (main) where

import LaunchTUI (launch)

main :: IO ()
main = launch

module TestSpec (main) where

import Test.Hspec
import GeometrySpec
import TopologySpec
import EntitySpec

main :: IO ()
main = do hspec GeometrySpec.spec
          hspec TopologySpec.spec
          hspec EntitySpec.spec

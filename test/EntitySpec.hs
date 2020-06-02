module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E

nullE :: E.Entity Float
nullE = E.nullEntity

spec :: Spec
spec = do
    describe "emptyWorkspace" $ do
        it "Creates an Entity with zero sub-components" $
            (subCount nullE) `shouldBe` 0
    describe "addVertex" $ do
        it "Adds a single Vertex to the Entity" $
            let f   = length . E.getVertices
                e   = nullE
                xs  = f e
                xs' = f $ E.addVertex e 0 0 0
            in (xs' - xs)  `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            E.getComponents v `shouldBe` (x, y, z)
                where (x,y,z) = (10, 20, 0)
                      e       = E.addVertex nullE x y z
                      v       = last $ E.getVertices e

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws

module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo

nullE :: E.Entity Float
nullE = E.nullEntity

spec :: Spec
spec = do
    describe "nullEntity" $ do
        it "Creates an Entity with zero sub-components" $
            (subCount nullE) `shouldBe` 0
    describe "addVertex" $ do
        it "Adds a single Vertex to the Entity" $
            let vs = E.getVertices $ E.addVertex nullE p
                p  = Geo.makePoint 0 0 0
            in length vs `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            let p = Geo.makePoint 10 20 0
                v = last . E.getVertices $ E.addVertex nullE p
            in E.getPoint v `shouldBe` p

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws

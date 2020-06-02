module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo

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
                xs' = f $ E.addVertex e (Geo.makePoint 0 0 0)
            in (xs' - xs)  `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            E.getPoint v `shouldBe` p
                where p       = Geo.makePoint 10 20 0
                      v       = last . E.getVertices $ E.addVertex nullE

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
subCount :: E.Entity a -> Int
subCount ws = vs + es
    where vs = (length . E.getVertices) ws
          es = (length . E.getEdges) ws

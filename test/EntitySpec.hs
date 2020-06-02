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
            let vs = (length . E.getVertices) nullE
                es = (length . E.getEdges) nullE
            in (vs + es) `shouldBe` 0
    describe "addVertex" $ do
        let e = E.addVertex nullE p
            p = Geo.makePoint 10 20 0
            v = last . E.getVertices $ e
        it "Adds a single Vertex to the Entity" $
            (length $ E.getVertices e) `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            E.getPoint v `shouldBe` p
        it "allows Vertex to be retrieved using Geometry" $
            E.getVertex e p `shouldBe` Just v

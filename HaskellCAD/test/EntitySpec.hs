module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo
import Linear.V3

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
            p = V3 10 20 0
            v = last . E.getVertices $ e
        it "Adds a single Vertex to the Entity" $
            (length $ E.getVertices e) `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            E.getPoint e v `shouldBe` Just p
        it "allows Vertex to be retrieved using Geometry" $
            E.getVertex e p `shouldBe` Just v
    describe "addEdge" $ do
        let e  = E.addEdge e0 v1 p2
            e0 = E.addVertex nullE p1
            v1 = last . E.getVertices $ e0
            p1 = V3 10 10 10
            p2 = V3 20 20 20
        it "Adds a single Edge to the Entity" $
            (length $ E.getEdges e) `shouldBe` 1
        it "Adds a single Vertex to Entity" $
            (length $ E.getVertices e) `shouldBe` 2
        it "Creates a line from v1 to the newly created Vertex" $
            let line = Geo.makeLine p1 p2
                ed   = last . E.getEdges $ e
            in (E.getCurve e ed) `shouldBe` Just line
    describe "oppositeVertex" $ do
        let e  = E.addEdge (E.addVertex nullE p1) v1 p2
            [v2,v1] = E.getVertices e
            [ed] = E.getEdges e
            p1 = V3 10 20 30
            p2 = V3 5 10 5
        it "Returns the Vertex on the other side of the Edge" $
            (E.oppositeVertex e v1 ed) `shouldBe` Just v2

module EntitySpec (spec) where

import Test.Hspec
import qualified Entity as E
import qualified Geometry as Geo
import Linear.V3
import Control.Monad.State

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
        let (v, e) = runState (E.addVertex p) nullE
            p = V3 10 20 0
        it "Adds a single Vertex to the Entity" $
            (length $ E.getVertices e) `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            E.getPoint e v `shouldBe` Just p
        it "allows Vertex to be retrieved using Geometry" $
            v `shouldSatisfy` (`elem` E.getVertex e p)
    describe "addEdge" $ do
        let ((_, _, edge), entity) = runState estate nullE
            p1 = V3 10 10 10
            p2 = V3 20 20 20
            estate = do
                 _v1   <- E.addVertex p1
                 _v2   <- E.addVertex p2
                 _edge <- E.addEdge _v1 _v2
                 pure (_v1, _v2, _edge)
        it "Adds a single Edge to the Entity" $
            (length $ E.getEdges entity) `shouldBe` 1
        it "Does not modify the number of Vertex in the Entity" $
            (length $ E.getVertices entity) `shouldBe` 2
        it "Creates a line from v1 to v2" $ do
            let line = Geo.makeLine p1 p2
            E.getCurve entity edge `shouldBe` Just line
    --describe "oppositeVertex" $ do
        --let (edge, entity) = runState (E.addEdge p1 p2) nullE
            --p1 = V3 10 20 30
            --p2 = V3 5 10 5
            --v1 = 
        --it "Returns the Vertex on the other side of the Edge" $
            --Just (E.oppositeVertex e1 v1 edge) `shouldBe` Just v2

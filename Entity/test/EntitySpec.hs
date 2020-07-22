module EntitySpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Entity
import qualified Geometry as Geo
import Linear.V3 (V3(V3))
import Control.Monad.State (runState)

nullE :: Entity Float
nullE = nullEntity

spec :: Spec
spec = do
    describe "addVertex" $ do
        let (v, e) = runState (addVertex p) nullE
            p = V3 10 20 0
        it "Adds a single Vertex to the Entity" $
            (length $ getVertices e) `shouldBe` 1
        it "Creates a Vertex at the given Geometry" $
            getPoint e v `shouldBe` Just p
        it "allows Vertex to be retrieved using Geometry" $
            v `shouldSatisfy` (`elem` getVertex e p)
    describe "addEdge" $ do
        let ((_, _, edge), entity) = runState estate nullE
            p1 = V3 10 10 10
            p2 = V3 20 20 20
            estate = do
                 _v1   <- addVertex p1
                 _v2   <- addVertex p2
                 _edge <- addEdge _v1 _v2
                 pure (_v1, _v2, _edge)
        it "Adds a single Edge to the Entity" $
            (length $ getEdges entity) `shouldBe` 1
        it "Does not modify the number of Vertex in the Entity" $
            (length $ getVertices entity) `shouldBe` 2
        it "Creates a line from v1 to v2" $ do
            let line = Geo.makeLine p1 p2
            getCurve entity edge `shouldBe` Just line
    --describe "oppositeVertex" $ do
        --let (edge, entity) = runState (addEdge p1 p2) nullE
            --p1 = V3 10 20 30
            --p2 = V3 5 10 5
            --v1 = 
        --it "Returns the Vertex on the other side of the Edge" $
            --Just (oppositeVertex e1 v1 edge) `shouldBe` Just v2

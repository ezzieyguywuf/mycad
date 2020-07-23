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
        let (vertex, entity) = runState (addVertex point) nullE
            point = V3 10 20 0
        it "Creates a Vertex at the given Geometry" $
            getPoint entity vertex `shouldBe` Just point
        it "allows Vertex to be retrieved using Geometry" $
            vertex `shouldSatisfy` (`elem` getVertex entity point)
    describe "addEdge" $ do
        let (Just edge, entity) = runState (prep >>= run) nullE
            p1 = V3 10 10 10
            p2 = V3 20 20 20
            prep = do v1 <- addVertex p1
                      v2 <- addVertex p2
                      pure (v1, v2)
            run = uncurry addEdge
        it "Creates a line from v1 to v2" $ do
            let line = Geo.makeLine p1 p2
            getCurve entity edge `shouldBe` Just line
        --it "Returns Nothing if the two vertices are the same" $
    --describe "oppositeVertex" $ do
        --let (edge, entity) = runState (addEdge p1 p2) nullE
            --p1 = V3 10 20 30
            --p2 = V3 5 10 5
            --v1 = 
        --it "Returns the Vertex on the other side of the Edge" $
            --Just (oppositeVertex e1 v1 edge) `shouldBe` Just v2

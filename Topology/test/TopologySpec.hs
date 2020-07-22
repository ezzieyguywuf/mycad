module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Topology as T
import Control.Monad.State

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Is inversed by removeVertex, resulting in original state" $
            property (prop_addRemoveIdentity (T.addFreeVertex >>= T.removeVertex))
        it "Creates one new Vertex" $
            property (prop_addsOneVertex T.addFreeVertex)
        it "Does not modify the Edges" $
            property (prop_doesNotModifyEdges T.addFreeVertex)
        it "Does not modify the Faces" $
            property (prop_doesNotModifyFaces T.addFreeVertex)
    --describe "addEdge" $ do
        --let prep = do
                --v1 <- T.addFreeVertex
                --v2 <- T.addFreeVertex
                --pure (v1, v2)
            --run = uncurry T.addEdge
            --remove = T.removeEdge
        --it "Is inversed by removeEdge, resulting in the orignal state" $
            --property (prop_addRemoveIdentity' prep (run >=> remove))
        --it "Does not add or remove any Vertex" $
            --property (prop_doesNotAddOrRemoveVertices prep run)
        ---- it "Makes each previously \"open\" vertex \"closed\""
        --it "Adds a single Edge" $
            --property (prop_addsOneEdge (prep >>= run))
        --it "does not modify the Faces" $
            --property (prop_doesNotModifyFaces $ prep >>= run)
-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_addRemoveIdentity :: T.TopoState a -> T.Topology -> Bool
prop_addRemoveIdentity run t = evalState test t
    where test = do
            s  <- get
            s' <- run >> get
            pure $ s == s'

--prop_addRemoveIdentity' :: T.TopoState a -> TopoMod a b-> T.Topology -> Bool
--prop_addRemoveIdentity' prep run t = evalState test t
    --where test = do
            --a <- prep
            --s <- get
            --s'<- (run a) >> get
            --pure $ s == s'

prop_addsOneVertex :: T.TopoState a -> T.Topology -> Bool
prop_addsOneVertex = deltaXIsN T.getVertices 1

--prop_addsOneEdge :: T.TopoState a -> T.Topology -> Bool
--prop_addsOneEdge = deltaXIsN T.getEdges 1

--prop_doesNotAddOrRemoveVertices :: T.TopoState a -> TopoMod a b -> T.Topology -> Bool
--prop_doesNotAddOrRemoveVertices = prep_deltaXIsN T.getVertices 0

prop_doesNotModifyEdges :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyEdges = doesNotModifyX T.getEdges

prop_doesNotModifyFaces :: T.TopoState a -> T.Topology -> Bool
prop_doesNotModifyFaces = doesNotModifyX T.getFaces

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
type TopoGetter a = T.Topology -> [a]
type TopoMod a b = a -> T.TopoState b
deltaXIsN :: TopoGetter b -> Int -> T.TopoState a -> T.Topology -> Bool
deltaXIsN getter n run initial = prep_deltaXIsN getter n noPrep run' initial
    where noPrep = pure id
          run'  = \_ -> run

prep_deltaXIsN :: TopoGetter a -> Int -> T.TopoState b -> TopoMod b c -> T.Topology -> Bool
prep_deltaXIsN getter n prep run initial = evalState test initial
    where test = do
            b <- prep
            xs <- gets (length . getter)
            xs' <- run b >> gets (length . getter)
            pure (xs' - xs == n)

doesNotModifyX :: Eq a => TopoGetter a -> T.TopoState b -> T.Topology -> Bool
doesNotModifyX getter run initial = evalState test initial
    where test = do
            xs <- gets getter
            xs' <- run >> gets getter
            pure (xs' == xs)

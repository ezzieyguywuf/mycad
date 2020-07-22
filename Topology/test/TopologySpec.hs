module TopologySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Topology
import Control.Monad.State

spec :: Spec
spec = do
    describe "addFreeVertex" $ do
        it "Is inversed by removeVertex, resulting in original state" $
            property (prop_stateIdentity (addFreeVertex >>= removeVertex))

-- ===========================================================================
--                            Properties
-- ===========================================================================
prop_stateIdentity :: TopoState a -> TestTopology -> Bool
prop_stateIdentity run (TestTopology topology )= evalState test topology
    where test = do
            s  <- get
            s' <- run >> get
            pure $ s == s'

-- ===========================================================================
--                            Helper Functions
-- ===========================================================================
-- | In order to fix an orphaned instance warning, we'll wrap this in newtype
--   before defining a new instance
newtype TestTopology = TestTopology Topology deriving (Show)

-- | This will generate a random Topology
instance Arbitrary TestTopology where
    arbitrary = elements $ fmap TestTopology [t0, t1, t2]
        where t0 = emptyTopology
              t1 = execState addFreeVertex emptyTopology
              t2 = execState addFreeVertex t1

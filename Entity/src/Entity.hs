{-|
Module      : Entity
Description : An Entity is a 3-dimensional object with adjacency knowledge
Copyright   : (c) Wolfgang E. Sanyer, 2020
License     : MPL2
Maintainer  : WolfgangESanyer@Gmail.com
Stability   : experimental
Portability : POSIX

Specifically, we can think of Geometry and Topology as two distinct concepts: whereas
Geometry deals with Points, Curves, and Surfaces in euclidean space, Topology deals with
Vertices, Edges, ad Faces in a "graph".

What does this mean? Well, a piece of Geometry tells you where something is, or how big it
is, or if it intersects with something else

A piece of Topology, on the other hand, tells you whether or not two things are connected,
i.e. "neighbors".

By putting these two concepts together in the form of an Entity, we have the essential
building block for a Computer Aided Design, or CAD, software.

Please note that there could be some confusion between an "Entity.Vertex" and
"Topology.Vertex", and similarly an "Entity.Edge" and a "Topology.Edge". A
distinct decision was made to leave the nomenclature this way - at the end of
the day, there are rather ambiguous definitions for Vertex, Edge, and even
Face. There does not seem to be a clear concensus as to whether these entities
should be strictly topological or include geometric information.V

In the future, it is expected that there will be other "Entity" data types that
far suprass any amount of "Topology" data types available. For example,
"EdgeLoop", "Solid", "ExtrudedSolid", etc... This should make it clear that the
data types in "Topology" are expected to be the very primitive foundations of a
BREP representation, while the data types in "Entity" are intended to build
upon the relational information in "Topology", using "Geometry" as necessary to
make useful data.
-}
module Entity
( -- * Exported Types
  Entity
, EntityState
  -- * Creation and Modification
, nullEntity
, emptyEntityState
, addVertex
, addEdge
  -- * Inspection
, getPoint
, getCurve
, getCurves
, vertexFromID
  -- * Pretty Printing
, prettyPrintEntity
) where

-- Third-party
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc (Doc, pretty, line, viaShow, vsep)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.State (State, get, gets, runState, evalState, put)

-- Internal
import qualified Geometry as Geo
import qualified Topology as Topo
import Topology.PrettyPrint (prettyPrintTopology, prettyShowVertex, prettyShowEdge)

-- ===========================================================================
--                               Data Types
-- ===========================================================================
-- | This maps a "Topology.Vertex" to a specfic "Geometry.Point".
type VertexMap p = Map.Map Topo.Vertex (Geo.Point p)

-- | This maps a "Topology.Edge" to a specific "Geometry.Curve"
type EdgeMap p = Map.Map Topo.Edge (Geo.Line p)

-- | The main data type, encapsulating an entire "Topology" and associated
--   "Geometry"
--
--   Please note that the "Vertex" and "Edge" stored here are distinctly *not*
--   "Topology.Vertex" nor "Topology.Edge". Rather, these are primitive
--   "Entity" data types that encapsulate both "Topology" and "Geometry"
data Entity p = Entity { getVertexMap :: VertexMap p
                       , getEdgeMap   :: EdgeMap p
                       , _getTopology :: Topo.Topology
                       } deriving (Show, Eq)

-- | The will carry the state of an Entity, parametrized over Geo.Point type @p@
type EntityState p = State (Entity p)

-- ===========================================================================
--                       Exported Free Functions
-- ===========================================================================


-- | Returns an Entity that has nothing in it.
nullEntity :: Entity a
nullEntity = Entity Map.empty Map.empty Topo.emptyTopology

emptyEntityState :: EntityState a ()
emptyEntityState = pure ()

-- | Adds a "Vertex" to the "Entity".
--
--   A "Vertex" has both a "Geometry" (a "Point"), and a "Topology" (a
--   "Topology.Vertex")
addVertex :: Fractional a => Geo.Point a -> EntityState a Topo.Vertex
addVertex p = do
    -- First, retrieve the current state
    (Entity vmap emap t) <- get

    let -- Add a Vertex to the Topology
        (v, t')  = runState Topo.addFreeVertex t
        -- Pair the topological Vertex with the provided geometric Point
        vmap'    = Map.insert v p vmap

    -- Update our state with the new information
    put (Entity vmap' emap t')

    -- Give the user the generated "Topology.Vertex"
    pure v

-- | Adds an Edge to the Entity.
--
--   If either or both of the supplied "Topology.Vertex" are not currently
--   present in the Entity, Nothing is returned.
--
--   The created Edge will geometrically have a straight line between the two
--   Vertex
addEdge :: (Fractional a, Eq a)
        => Topo.Vertex
        -> Topo.Vertex
        -> EntityState a (Either String Topo.Edge)
addEdge v1 v2 = runExceptT $ do
    -- First, retrieve the current state
    (Entity vmap emap topology) <- lift get

    -- Try to retrieve the points associated with these Vertices
    p1 <- lift (getPoint' v1) >>= note ("Can't find point for " <> (show v1))
    p2 <- lift (getPoint' v2) >>= note ("Can't find point for " <> (show v2)) 

    -- Bail out if the two points are geometrically equivalent - how would you
    -- make a line then?!
    when (p1 == p2) (throwError "Cannot add an Edge when p1 == p2")
        -- :: ExceptT String (EntityState p) ()

    -- Try to add the given Edge to the Topology
    (edge, t1) <- case runState (Topo.addEdge v1 v2) topology of
                      (Nothing, _)    -> throwError "Could not add Edge to topology"
                      (Just edge, t') -> pure (edge, t')

    let -- We'll make a geometric straight line between the two points
        geoline = Geo.makeLine p1 p2
        -- Pair the topological Edge with the line we made earlier
        emap' = Map.insert edge geoline emap

    -- Update our state with the new information
    lift (put (Entity vmap emap' t1))

    -- Give the user a reference to the new Edge
    pure edge

-- | Turns a `Maybe a` into in `ExceptT e a`, where `e` is the error provided.
note :: MonadError e m => e -> Maybe a -> m a
note msg = maybe (throwError msg) pure

-- | Returns the underlying geometric Point of the Vertex
--
--   Returns Nothing if the Vertex is not part of this Entity
getPoint :: (Fractional a, Eq a) => Entity a -> Topo.Vertex -> Maybe (Geo.Point a)
getPoint entity vertex = evalState (getPoint' vertex) entity

-- | A stateful version of "getPoint".
getPoint' :: (Fractional a, Eq a) => Topo.Vertex -> EntityState a (Maybe (Geo.Point a))
getPoint' vertex = Map.lookup vertex <$> gets getVertexMap

-- | Returns the underlying geometric "Curve" of the "Edge'"
getCurve :: Entity a -> Topo.Edge -> Maybe (Geo.Line a)
getCurve e = (`Map.lookup` getEdgeMap e)

-- | Returns all the geometrice Curves in the Entity
getCurves :: Entity a -> [Geo.Line a]
getCurves = Map.elems . getEdgeMap

-- | Returns the Vertex with the given VertexID
vertexFromID :: Int -> EntityState p (Maybe Topo.Vertex)
vertexFromID n = evalState (Topo.vertexFromID n) <$> gets _getTopology

prettyPrintEntity :: Show p => Entity p -> Doc ()
prettyPrintEntity (Entity vs es topology) = doc
    where doc = vsep (vertices <> edges) <> line <> topo
          vertices = fmap (maybeDoc prettyShowVertex') (Map.assocs vs)
          edges = fmap (maybeDoc prettyShowEdge') (Map.assocs es)
          topo = prettyPrintTopology topology
          prettyShowVertex' v = evalState (prettyShowVertex v) topology
          prettyShowEdge' e = evalState (prettyShowEdge e) topology

maybeDoc :: (Show k, Show v) => (k -> Maybe (Doc ())) -> (k,v) -> Doc ()
maybeDoc func (key, value) = case func key of
                        Just doc -> doc <> pretty ": " <> viaShow value
                        Nothing  -> pretty $ "Unable to prettify " <> show key
-- ===========================================================================
--                       Private Free Functions
-- ===========================================================================

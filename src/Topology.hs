module Topology
( -- | Exported types
  Vertex(..)
, Edge(..)
, Face(..)
, Topology(..)
, VertexID(VertexID)
, EdgeID(EdgeID)
, FaceID(FaceID)
, EdgeLoopID(EdgeLoopID)
  -- | Exported functions
, addFreeVertex
, addRayEdge
, addLoopEdge
, addChordEdge
, addOpenEdgeLoop
, addEdgeToLoop
, getVertices
, getEdges
, getFaces
, getEdgeLoops
, vertexEdges
, edgeVertices
, edgeFaces
, pprTopo
--, printTopology
--, addEdge
  -- | These come from Type Classes
)where

import Test.QuickCheck
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc

--import qualified Data.Text as Text

-- ===========================================================================
--                               Data Types
-- ===========================================================================
data Vertex =
     FreeVertex
   | RayVertex EdgeID
   | ChordVertex EdgeID
   | LoopVertex EdgeID FaceID
   deriving (Show, Eq)

data Edge   =
      RayEdge VertexID
    | LoopEdge VertexID FaceID
    | ChordEdge VertexID VertexID
    deriving (Show, Eq)

data EdgeLoop = OpenEdgeLoop VertexID deriving (Show, Eq)

data Face   =
      LoopFace VertexID EdgeID
    deriving (Show, Eq)

data Topology =
      EmptyTopology
    | Topology Vertices Edges Faces EdgeLoops
    deriving (Show, Eq)

newtype EdgeID   = EdgeID Int deriving (Show, Eq, Ord)
newtype EdgeLoopID   = EdgeLoopID Int deriving (Show, Eq, Ord)
newtype VertexID = VertexID Int deriving (Show, Eq, Ord)
newtype FaceID   = FaceID Int deriving (Show, Eq, Ord)

type Vertices  = Map.Map VertexID Vertex
type Edges     = Map.Map EdgeID Edge
type EdgeLoops = Map.Map EdgeLoopID EdgeLoop
type Faces     = Map.Map FaceID Face

-- ===========================================================================
--                               Free Functions
-- ===========================================================================
addFreeVertex :: Topology -> Topology
addFreeVertex t =
    let (vs, es, fs, els)  = getAll t
        vid = VertexID $ length vs
        v   = FreeVertex
        vs' = Map.insert vid v vs
    in Topology vs' es fs els

addRayEdge :: Topology -> Topology
addRayEdge t =
    let (vs, es, fs, els)  = getAll t
        vid = VertexID $ length vs
        eid = EdgeID $ length es
        vs' = Map.insert vid (RayVertex eid) vs
        es' = Map.insert eid (RayEdge vid) es
    in (Topology vs' es' fs els)

addLoopEdge :: Topology -> Topology
addLoopEdge t =
    let (vs, es, fs, els)  = getAll t
        vid = VertexID $ length vs
        eid = EdgeID $ length es
        fid = FaceID $ length fs
        v   = LoopVertex eid fid
        e   = LoopEdge vid fid
        f   = LoopFace vid eid
        vs' = Map.insert vid v vs
        es' = Map.insert eid e es
        fs' = Map.insert fid f fs
    in Topology vs' es' fs' els

addChordEdge :: Topology -> Topology
addChordEdge t =
    let (vs, es, fs, els) = getAll t
        vid  = VertexID $ length vs
        vid2 = VertexID $ length vs + 1
        eid  = EdgeID $ length es
        v1   = ChordVertex eid
        v2   = ChordVertex eid
        e    = ChordEdge vid vid2
        vs'  = Map.insert vid2 v2 $ Map.insert vid v1 vs
        es'  = Map.insert eid e es
    in Topology vs' es' fs els

addOpenEdgeLoop :: Topology -> Topology
addOpenEdgeLoop t =
    let (vs, es, fs, els) = getAll (addChordEdge t)
        (vid, _) = Map.findMax vs
        elid = EdgeLoopID $ length els
        els' = Map.insert elid (OpenEdgeLoop vid) els
    in Topology vs es fs els'

addEdgeToLoop :: EdgeLoopID -> Topology -> Topology
addEdgeToLoop elid t =
    let (vs, es, fs, els) = getAll (addFreeVertex t)
        eid = EdgeID $ length es
        (OpenEdgeLoop v1) = els Map.! elid
        (v2, _) = Map.findMax vs
        vs' = Map.insert v2 (ChordVertex eid) vs
        es' = Map.insert eid (ChordEdge v1 v2) es
        els' = Map.insert elid (OpenEdgeLoop v2) els
    in Topology vs' es' fs els'

getVertices :: Topology -> Vertices
getVertices EmptyTopology = Map.empty
getVertices (Topology vs _ _ _) = vs

getEdges :: Topology -> Edges
getEdges EmptyTopology = Map.empty
getEdges (Topology _ es _ _) = es

getFaces :: Topology -> Faces
getFaces EmptyTopology = Map.empty
getFaces (Topology _ _ fs _) = fs

getEdgeLoops :: Topology -> EdgeLoops
getEdgeLoops EmptyTopology = Map.empty
getEdgeLoops (Topology _ _ _ els ) = els

vertexEdges :: VertexID -> Topology -> Maybe Edges
vertexEdges vid t = do
    let vs = getVertices t
        es = getEdges t
    v <- Map.lookup vid vs
    case v of
        (RayVertex eid) -> Just $ filterSubMap [eid] es
        (ChordVertex eid) -> Just $ filterSubMap [eid] es
        (LoopVertex eid _) -> Just $ filterSubMap [eid] es
        FreeVertex -> Nothing

edgeVertices :: EdgeID -> Topology -> Maybe Vertices
edgeVertices eid t = do
    let vs = getVertices t
        es = getEdges t
    e <- Map.lookup eid es
    case e of
        (RayEdge vid) -> Just $ filterSubMap [vid] vs
        (LoopEdge vid _) -> Just $ filterSubMap [vid] vs
        (ChordEdge v1 v2) -> Just $ filterSubMap [v1, v2] vs

edgeFaces :: EdgeID -> Topology -> Maybe Faces
edgeFaces eid t = do
    let es = getEdges t
        fs = getFaces t
    e <- Map.lookup eid es
    case e of
        (LoopEdge _ fid) -> Just $ filterSubMap [fid] fs
        _ -> Nothing

--printTopology :: Topology -> Text.Text
--printTopology t =
    --let (vs, es, fs, els) = getAll t
    --in printMap vs

-- | For pretty printing
pprTopo :: Topology -> Doc ann
pprTopo t =
    case t of
      EmptyTopology -> pretty "EmptyTopology"
      (Topology vs es fs els) ->
          (pprVMap "Vertices" vs) <+> line
          <+> (pprEMap "Edges" es) <+> line
          <+> (pprFMap "Faces" fs) <+> line
          <+> (pprELMap "EdgeLoops" els)

pprVMap :: String -> Map.Map VertexID Vertex -> Doc ann
pprVMap n m = printMap n m pprVTuple

pprEMap :: String -> Map.Map EdgeID Edge -> Doc ann
pprEMap n m = printMap n m pprETuple

pprFMap :: String -> Map.Map FaceID Face -> Doc ann
pprFMap n m = printMap n m pprFTuple

pprELMap :: String -> Map.Map EdgeLoopID EdgeLoop -> Doc ann
pprELMap n m = printMap n m pprELTuple

pprVTuple :: (VertexID, Vertex) -> Doc ann
pprVTuple (vid, v) = printTuple vid v pprVID pprVertex

pprETuple :: (EdgeID, Edge) -> Doc ann
pprETuple (eid, e) = printTuple eid e pprEID pprEdge

pprFTuple :: (FaceID, Face) -> Doc ann
pprFTuple (fid, f) = printTuple fid f pprFID pprFace

pprELTuple :: (EdgeLoopID, EdgeLoop) -> Doc ann
pprELTuple (elid, el) = printTuple elid el pprELID pprELoop

pprVertex :: Vertex -> Doc ann
pprVertex v =
    case v of
        FreeVertex -> pretty "FreeV"
        (RayVertex eid) -> pretty "RayV" <+> pprEID eid
        (ChordVertex eid) -> pretty "ChordV" <+> pprEID eid
        (LoopVertex eid fid) -> pretty "ChordV" <+> pprEID eid <+> pprFID fid

pprEdge :: Edge -> Doc ann
pprEdge e =
    case e of
        (RayEdge vid) -> pretty "RayE" <+> pprVID vid
        (LoopEdge vid fid) -> pretty "LoopE" <+> pprVID vid <+> pprFID fid
        (ChordEdge v1 v2) -> pretty "ChordE" <+> pprVID v1 <+> pprVID v2

pprFace :: Face -> Doc ann
pprFace (LoopFace vid eid) = pretty "LoopF" <+> pprVID vid <+> pprEID eid

pprELoop :: EdgeLoop -> Doc ann
pprELoop (OpenEdgeLoop vid) = pretty "ELoop" <+> pprVID vid

pprVID :: VertexID -> Doc ann
pprVID (VertexID n) = printID "V" n

pprEID :: EdgeID -> Doc ann
pprEID (EdgeID n) = printID "E" n

pprFID :: FaceID -> Doc ann
pprFID (FaceID n) = printID "F" n

pprELID :: EdgeLoopID -> Doc ann
pprELID (EdgeLoopID n) = printID "EL" n

-- ===========================================================================
--                       Private Free Functions
-- ===========================================================================
filterSubMap :: (Ord k, Eq a) => [k] -> Map.Map k a -> Map.Map k a
filterSubMap k m = Map.filterWithKey (\x _ -> x `elem` k) m

getAll :: Topology -> (Vertices, Edges, Faces, EdgeLoops)
getAll t = (getVertices t, getEdges t, getFaces t, getEdgeLoops t)

printID :: String -> Int -> Doc ann
printID h n = pretty h <> pretty n

printTuple :: a -> b -> (a -> Doc ann) -> (b -> Doc ann) -> Doc ann
printTuple a b fa fb = fa a <> pretty ":" <+> fb b

printMap :: (Ord k, Eq a) =>
        String
        -> Map.Map k a
        -> ((k, a) -> Doc ann)
        -> Doc ann
printMap n m f = (pretty n) <+> (align . vsep) (map f (Map.assocs m))

-- ===========================================================================
--                               Type Classes
-- ===========================================================================
-- ===========================================================================
--                             Implementations
-- ===========================================================================
instance Arbitrary Topology where
    arbitrary = do
        let t0  = EmptyTopology
            t1  = addFreeVertex t0
            t2  = addRayEdge t0
            t3  = addLoopEdge t0
        elements [t0, t1, t2, t3]

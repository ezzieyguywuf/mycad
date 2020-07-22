-- * Pretty printing
-- | In the output, '->' means "out of" and '<-' means "in to". Therefore:
--
-- >>> prettyPrintVertex v t
-- V0: <- None
--     -> E0
--
-- Means that the 'Vertex' @V0@ has __no__ entities ('Edge' or otherwise)
-- pointing in to it, and a single 'Edge' named @E0@ pointing out of it.
module Topology.PrettyPrint
(

)where
-- from: Tanisha
--beautifulPrintEdge.run

-- Third-Party
import Data.Text.Prettyprint.Doc
import qualified Data.Graph.Inductive.Graph as Graph

-- Internal
import Topology (Topology, Vertex(..), Edge(..), Face(..))

prettyPrintVertex :: Topology -> Vertex -> Doc ann
prettyPrintVertex t (Vertex i) = prettyPrintNodeWithNeighbors t i

prettyPrintEdge :: Topology -> Edge -> Doc ann
prettyPrintEdge t (Edge i) = prettyPrintNodeWithNeighbors t i

prettyPrintFace :: Topology -> Face -> Doc ann
prettyPrintFace t (Face i) = prettyPrintNodeWithNeighbors t i

prettyPrintTopology :: Topology -> Doc ann
prettyPrintTopology t = 
    case show doc of
        "" -> pretty "empty topology"
        _  -> doc
    where vs = prettyPrintMany t prettyPrintVertex getVertices
          es = prettyPrintMany t prettyPrintEdge getEdges
          fs = prettyPrintMany t prettyPrintFace getFaces
          func d = if show d == "" then d else d <> line
          doc = foldMap func [vs,es,fs]

-- ===========================================================================
--                        Private Free Functions
-- ===========================================================================
prettyPrintNodeWithNeighbors :: Topology -> Int -> Doc ann
prettyPrintNodeWithNeighbors t i = h <> pretty ":" <+> ns
    where h   = prettyPrintNodeLabel lab
          lab = fromJust $ Graph.lab t' i
          ns  = prettyPrintNeighbors i t
          t'  = unTopology t

prettyPrintMany ::
    Topology -> (Topology -> a -> Doc ann) -> (Topology -> [a]) -> Doc ann
prettyPrintMany topo printer getter = vsep $ map (printer topo) $ getter topo

prettyPrintNeighbors :: Int -> Topology -> Doc ann
prettyPrintNeighbors i t = align doc
    where pns = Graph.pre t' i
          sns = Graph.suc t' i
          lns = [length pns, length sns]
          t'  = unTopology t
          doc | all (== 0) lns = pretty "No neighbors"
              | otherwise = pre <> line <> suc
              where pre = prettyPrintNodes "<-" pns t
                    suc = prettyPrintNodes "->" sns t

prettyPrintNodes :: String -> [Int] -> Topology -> Doc ann
prettyPrintNodes h ns t = pretty h <+> (align . vsep) ns'
    where ns' | length ns == 0 = [pretty "None"]
              | otherwise = map prettyPrintNodeLabel $ mapMaybe (Graph.lab t') ns
          t' = unTopology t

prettyPrintNodeLabel :: NodeLabel -> Doc ann
prettyPrintNodeLabel (e, n) = prettyPrintElement e <> pretty n

prettyPrintElement :: Element -> Doc ann
prettyPrintElement EVertex = pretty "V"
prettyPrintElement EEdge   = pretty "E"
prettyPrintElement EFace   = pretty "F"


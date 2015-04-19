module VDPT
    (
        module VDPT.Types
    ,   numberTraceTree 
    ,   nodeMap
    ,   nodeParentsMap 
    ,   traceTypeCounts 
    ) where

import Data.Tree
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Data.IntMap.Lazy as IL
import qualified Data.Map.Strict as M
import qualified Data.Set.Strict as S
import Control.Comonad
import Control.Lens
import VDPT.Types

numberTraceTree :: Tree (NodeId -> Attributes) -> Tree Attributes
numberTraceTree = snd . flip TR.mapAccumL 0 (\i f -> (succ i, f i))

nodeMap :: Trace -> IL.IntMap Trace 
nodeMap
    = F.foldMap (\n -> IL.singleton (_nodeId . rootLabel $ n) (Trace n)) 
    . duplicate
    . getTrace

nodeParentsMap :: Trace -> IL.IntMap [(Int,Trace)]
nodeParentsMap (Trace t) = para go t
  where
    go n submaps
        = IL.insert (_nodeId . rootLabel $ n) [] 
        $ mconcat
        $ fmap (\(i, ml) -> fmap ((:) (i, Trace n)) ml) 
        $ zip [0..] submaps

traceTypeCounts :: Trace -> M.Map T.Text Int 
traceTypeCounts 
    = M.unionsWith (+)
    . map (\n -> M.singleton (_nodeType n) 1)
    . F.toList
    . getTrace

--directAncestorsByType :: Trace -> M.Map T.Text (S.Set Text)
--directAncestorsByType (Trace t) = para go t
--  where
--    go n submaps
--        = 
--        
--        $ unionsWith (<>)
--        (:)     


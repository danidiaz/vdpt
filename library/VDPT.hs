module VDPT
    (
        module VDPT.Types
    ,   numberTraceTree 
    ,   nodeMap
    ,   nodeParentsMap 
    ,   nodeParentIdsMap 
    ) where

import Data.Tree
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Data.IntMap.Lazy as IL
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

nodeParentsMap :: Trace -> IL.IntMap [Trace]
nodeParentsMap = para go . getTrace
  where
    go n submaps = IL.insert (_nodeId . rootLabel $ n) [] $ fmap ((:) (Trace n)) (mconcat submaps)
    
nodeParentIdsMap :: Trace -> IL.IntMap [NodeId]
nodeParentIdsMap = fmap (fmap (_nodeId . rootLabel . getTrace)) . nodeParentsMap

module VDPT
    (
        module VDPT.Types
    ,   numberTraceTree 
    ,   nodeMap
    ) where

import Data.Tree
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Data.IntMap.Lazy as IL
import Control.Comonad
import VDPT.Types

numberTraceTree :: Tree (NodeId -> Attributes) -> Tree Attributes
numberTraceTree = snd . flip TR.mapAccumL 0 (\i f -> (succ i, f i))

nodeMap :: Trace -> IL.IntMap Trace 
nodeMap
    = F.foldMap (\n -> IL.singleton (_nodeId . rootLabel $ n) (Trace n)) 
    . duplicate
    . getTrace


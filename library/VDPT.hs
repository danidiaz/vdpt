module VDPT
    (
        module VDPT.Types
    ,   numberTraceTree 
    ) where

import Data.Tree
import qualified Data.Traversable as TR

import VDPT.Types

numberTraceTree :: Tree (NodeId -> Attributes) -> Tree Attributes
numberTraceTree = snd . flip TR.mapAccumL 0 (\i f -> (succ i, f i))



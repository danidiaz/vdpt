module VDPT
    (
        module VDPT.Types
    ,   numberNodeTree 
    ,   nodeMap
    ,   nodeParentsMap 
    ,   nodeTypeCounts 
    ,   directAncestorsByType 
    ) where

import Data.Tree
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Data.IntMap.Lazy as IL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Comonad
import Control.Lens
import VDPT.Types

numberNodeTree :: Tree (NodeId -> Attributes) -> Tree Attributes
numberNodeTree = snd . flip TR.mapAccumL 0 (\i f -> (succ i, f i))

nodeMap :: Tree Attributes -> IL.IntMap (Tree Attributes)
nodeMap
    = F.foldMap (\n -> IL.singleton (_nodeId . extract $ n) n) 
    . duplicate

nodeParentsMap :: Tree Attributes -> IL.IntMap [(Int,Tree Attributes)]
nodeParentsMap t = para go t
  where
    go n submaps
        = IL.insert (_nodeId . extract $ n) [] 
        $ mconcat
        $ fmap (\(i, ml) -> fmap ((:) (i,n)) ml) 
        $ zip [0..] submaps

nodeTypeCounts :: Tree Attributes -> M.Map T.Text Int 
nodeTypeCounts 
    = M.unionsWith (+)
    . map (\n -> M.singleton (_nodeType n) 1)
    . F.toList

directAncestorsByType :: Tree Attributes -> M.Map T.Text (S.Set T.Text)
directAncestorsByType t = para go t
  where
    go n = M.unionsWith (<>) . (:) (setsForNode n) 
    setsForNode x = 
        let p = S.singleton. _nodeType . extract $ x 
            cs = map (flip M.singleton p . _nodeType . extract) (children x)
        in
        M.unionsWith (<>) cs

diff :: Tree Attributes -> Tree Attributes -> [(NodeId, NodeId, [Difference])]
diff t1@(Node r1 c1) t2@(Node r2 c2) = undefined 
  where
    cDiff = undefined






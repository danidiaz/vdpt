module VDPT
    (
        module VDPT.Types
    ,   numberNodeTree 
    ,   nodeMap
    ,   nodeParentsMap 
    ,   nodeTreeDiff
    ,   nodeTypes
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
import Control.Applicative
import Control.Monad
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

nodeTreeDiff :: Tree Attributes -> Tree Attributes -> [(NodeId, NodeId, [Difference])]
nodeTreeDiff t1@(Node r1 c1) t2@(Node r2 c2) =  
  case childCountDiff ++ typeDiff of
      ds@(_:_) -> mkDiffList ds
      [] -> case attrDiff of
          (_:_) -> mkDiffList attrDiff ++ subDiffs
          [] -> subDiffs
  where
    mkDiffList ds = [(_nodeId r1, _nodeId r2, ds)]   
    subDiffs = mconcat $ zipWith nodeTreeDiff c1 c2
    childCountDiff = 
        let (l1,l2) = (length c1, length c2) in
        guard (l1 /= l2) *> [DifferentNumberOfChildren l1 l2]
    typeDiff = 
        let (x1,x2) = (_nodeType r1, _nodeType r2) in 
        guard (x1 /= x2) *> [DifferentNodeTypes x1 x2]
    attrDiff = 
        let (attrs1, attrs2) = (_dynamic r1, _dynamic r2) in
        flip M.foldMapWithKey attrs1 $ \attrKey attrVal ->  
            case M.lookup attrKey attrs2 of
                Nothing -> [AttributeDissapeared attrKey] 
                Just attrVal2 ->  
                    guard (attrVal /= attrVal2) *> [AttributeChanged attrKey attrVal attrVal2]

nodeTypes :: Tree Attributes -> S.Set T.Text
nodeTypes = F.foldMap (S.singleton . _nodeType)   

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

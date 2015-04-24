{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module VDPT.Diagrams
    (
        module VDPT.Types
    ,   renderNodeTreeDia 
    ) where

import Data.Monoid
import Data.Tree
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
--import Control.Lens ((.~))

import Diagrams.Prelude
import Diagrams.TwoD.Size
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
import Lucid (renderBS)

import VDPT.Types

abbreviate :: Attributes -> T.Text 
abbreviate = mconcat . map (fst . T.splitAt 1) . T.words . _nodeType

renderNodeTreeDia :: Tree Attributes -> BL.ByteString
renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (mkHeight (750::Double)) [] "") . renderNodeTree
--renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (dims2D (1000::Float) 700) [] "") . renderNodeTree
--renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (mkWidth (500::Float)) [] "") . renderNodeTree


renderNodeTree :: Tree Attributes -> QDiagram SVG V2 Double Any
--renderNodeTree nodeTree = renderTree (\_ -> circle 1 # fc white `atop` text "FOO") arrowBetween (symmLayout nodeTree)
renderNodeTree nodeTree = renderTree 
    (\a -> letters2 a `atop` square 1 # fc white) 
    (~~) 
    (symmLayout' (with{ _slHSep = 3,  _slVSep = 2}) nodeTree)
  where
     letters a = text (T.unpack $ abbreviate a) # font "monospace" # fontSize (local 0.5) 
     number a = text (show . _nodeId $ a) # font "monospace" # fontSize (local 0.5) 
     letters2 a = position  [(p2 (0,0.25), letters a), (p2 (0,-0.25), number a)]  

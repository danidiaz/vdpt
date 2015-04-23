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

renderNodeTreeDia :: forall a. Tree a -> BL.ByteString
renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (mkHeight (750::Double)) [] "") . renderNodeTree
--renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (dims2D (1000::Float) 700) [] "") . renderNodeTree
--renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (mkWidth (500::Float)) [] "") . renderNodeTree

renderNodeTree :: Tree a -> QDiagram SVG V2 Double Any
--renderNodeTree nodeTree = renderTree (\_ -> circle 1 # fc white `atop` text "FOO") arrowBetween (symmLayout nodeTree)
renderNodeTree nodeTree = renderTree 
    (\_ -> text "FOO" # font "monospace" `atop` square 1 # fc white) 
    (~~) 
    (symmLayout' (with{ _slHSep = 3,  _slVSep = 3}) nodeTree)

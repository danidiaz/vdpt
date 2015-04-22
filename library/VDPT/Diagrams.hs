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

import Diagrams.Prelude
import Diagrams.TwoD.Size
import Diagrams.TwoD.Arrow
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG
import Lucid

import VDPT.Types

renderNodeTreeDia :: forall a. Tree a -> BL.ByteString
renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (dims2D (1000::Float) 700) [] "") . renderNodeTree
--renderNodeTreeDia = renderBS . renderDia SVG (SVGOptions (mkWidth (500::Float)) [] "") . renderNodeTree

renderNodeTree :: forall a b. Renderable (Path V2 Float) b => Tree a -> QDiagram b V2 Float Any
renderNodeTree nodeTree = renderTree (const (circle 1)) arrowBetween (symmLayout nodeTree)

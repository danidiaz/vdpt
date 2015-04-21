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
    ) where

import Data.Monoid
import Data.Tree
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative

import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG.CmdLine

import VDPT.Types


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module VDPT.Types where

import Data.Tree
import Data.Map.Strict
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty
import Control.Applicative
import Control.Lens

type NodeId = Int

data Attributes = Attributes 
    {
        _nodeId :: !NodeId
    ,   _nodeType :: !T.Text   
    ,   _dynamic :: !(Map T.Text JSON.Value)
    } deriving (Show, Eq)

$(makeLenses ''Attributes)


newtype Trace = Trace { getTrace :: Tree Attributes } deriving (Show, Eq)

traceRootId :: Trace -> NodeId
traceRootId = _nodeId . rootLabel . getTrace

traceRootType :: Trace -> T.Text
traceRootType = _nodeType . rootLabel . getTrace

instance JSON.ToJSON Trace where
    toJSON (Trace (Node attr children')) = 
        let subtrees = JSON.toJSON $ JSON.toJSON . Trace <$> children'
        in
        JSON.object 
            [ ("_id", JSON.toJSON $ _nodeId attr)
            , ("type", JSON.String $ _nodeType attr)
            , ("attributes", JSON.toJSON $ _dynamic attr)
            , ("children", subtrees)
            ]

traceKeyOrder :: T.Text -> T.Text -> Ordering
traceKeyOrder = keyOrder ["_id", "type", "attributes", "children"]

data Parsed = Parsed 
        {
            _original :: LT.Text
        ,   _parsedTrace :: Trace  
        } deriving (Show, Eq)


$(makeLenses ''Parsed)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module VDPT.Types where

import Data.Tree
import Data.Map.Strict
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty
import Control.Applicative
import Control.Lens
import GHC.Generics

type NodeId = Int

data Attributes = Attributes 
    {
        _nodeId :: !NodeId
    ,   _nodeType :: !T.Text   
    ,   _dynamic :: !(Map T.Text JSON.Value)
    } deriving (Show, Eq)

$(makeLenses ''Attributes)


newtype Trace = Trace { getTrace :: Tree Attributes } deriving (Show, Eq)


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

data Difference = 
      DifferentNumberOfChildren Int Int
    | DifferentNodeTypes T.Text T.Text
    | AttributeDissapeared T.Text
    | AttributeChanged T.Text JSON.Value JSON.Value
    deriving (Show, Eq, Generic) 

instance JSON.ToJSON Difference


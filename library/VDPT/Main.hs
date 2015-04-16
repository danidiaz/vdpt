module VDPT.Main (defaultMain) where

import Data.Monoid
import qualified Data.Foldable as F
import Control.Applicative
import qualified Options.Applicative as O

import VDPT.Server

data RootCommand = 
    Server Int -- server port
  deriving (Show)   

parserInfo :: O.ParserInfo RootCommand  
parserInfo = info' (Server <$> portP) "This is the main prog desc"
  where
    portP = O.option O.auto (O.value 3000 <> O.short 'p' <> O.long "port")

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)


defaultMain :: IO ()
defaultMain = do
    plan <- O.execParser parserInfo
    case plan of
        Server port -> server port


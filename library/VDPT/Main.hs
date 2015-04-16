module VDPT.Main (defaultMain) where

import Data.Monoid
import qualified Data.Foldable as F
import Control.Applicative
import qualified Options.Applicative as O

import VDPT.Server

data RootCommand = 
    Server Int -- server port
  | Client ClientCommand
  | Version
  deriving (Show)   

data ClientCommand = 
    Post
  | Assert
  | Delete
  deriving (Show)   

parserInfo :: O.ParserInfo RootCommand  
parserInfo = info' rootParser "This is the main prog desc"
  where
    rootParser :: O.Parser RootCommand 
    rootParser = subparser' 
        [ ("server", "Start server", Server <$> portP)
        , ("client", "Perform request against server", Client <$> clientParser)
        , ("version", "Print executable version", pure Version)
        ] 

    clientParser :: O.Parser ClientCommand 
    clientParser = subparser'
        [ ("upload", "Upload a trace", pure Post)
        , ("assert", "Perform an assertion over a trace", pure Assert)
        , ("delete", "Delete trace from server", pure Delete)
        ] 

    portP = O.option O.auto (O.value 3000 <> O.short 'p' <> O.long "port")

    subparser' = O.subparser . F.foldMap command'

    command' (cmdName,desc,parser) = O.command cmdName (info' parser desc)

    info' :: O.Parser a -> String -> O.ParserInfo a
    info' p desc = O.info (O.helper <*> p) (O.fullDesc <> O.progDesc desc)


defaultMain :: IO ()
defaultMain = do
    plan <- O.execParser parserInfo
    case plan of
        Version -> putStrLn version  
        Server port -> server port
        _ -> do
           putStrLn "No command-line client functionality has been implemented yet."
    return ()


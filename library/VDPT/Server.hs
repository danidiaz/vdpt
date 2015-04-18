{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module VDPT.Server
    (
        version
    ,   server
    ) where

import Data.Monoid
import Data.IORef
import qualified Data.Aeson as J
import Data.Aeson.Encode.Pretty
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.IntMap.Lazy as IL
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception
import Network
import Web.Scotty
import Network.Wai.Parse
import Network.HTTP.Types.Status
import Lucid
import VDPT
import VDPT.Parser

jsonViewURL :: T.Text
jsonViewURL = "https://chrome.google.com/webstore/detail/jsonview/chklaanhfefbnpoihckbnefhakgolnmc?hl=en"

version :: LT.Text
version = "0.1.0.0"

type TraceId = Int

makePages :: IO (IORef (Int, I.IntMap a))
makePages = newIORef (1, I.empty)

addPage :: a -> (Int, I.IntMap a) -> ((Int, I.IntMap a), Int)
addPage t (i, m) = ((succ i, I.insert i t m), i)

deletePage :: Int -> (Int, I.IntMap a) -> ((Int, I.IntMap a), ())
deletePage i' (i, m) = 
    let 
        deleted = I.delete i' m 
    in 
    (deleted `seq` (i, deleted), ())

deleteAllPages :: (Int, I.IntMap a) -> ((Int, I.IntMap a), ())
deleteAllPages (i, _) = ((i, I.empty), ())

textData :: ActionM LT.Text
textData = LT.decodeUtf8 <$> body

traceUploadBody :: ActionM LT.Text
traceUploadBody = do
    fmt <- requestFormat PlainTextRequestFormat 
    case fmt of
        PlainTextRequestFormat -> textData
        MultipartRequestFormat -> do
            mparam  <- M.lookup "tracetext". M.fromList <$> params  
            case mparam of 
                Just x -> return x
                Nothing -> LT.decodeUtf8 . fileContent . snd . head <$> files
        OtherRequestFormat -> liftIO $ throwIO $ userError "Unknown content format!" 

data RequestFormat = 
      MultipartRequestFormat
    | PlainTextRequestFormat
    | OtherRequestFormat
      
requestFormat :: RequestFormat -> ActionM RequestFormat
requestFormat defaultFormat = do
    mcontentHeader <- header "Content-Type"
    return $ case mcontentHeader of
        Just f -> parseContentHeader f  
        Nothing -> defaultFormat 
  where
    parseContentHeader fmt | LT.isInfixOf "multipart/form-data" fmt = MultipartRequestFormat
    parseContentHeader fmt | LT.isInfixOf "text/plain" fmt = PlainTextRequestFormat
    parseContentHeader _ =  OtherRequestFormat

data ResponseFormat = 
      HTMLFormat
    | JSONFormat
    | PlainTextFormat
    | SVGFormat
    | OtherFormat
    deriving (Show, Eq)

responseFormat :: ResponseFormat -> ActionM ResponseFormat
responseFormat defaultFormat = do
    -- explicit parameter has priority
    mformat <- M.lookup "$format" . M.fromList <$> params
    case mformat of
        Just f -> return $ case f of 
            "html" -> HTMLFormat
            "json" -> JSONFormat
            "text" -> PlainTextFormat 
            "svg" -> SVGFormat
            _ -> OtherFormat
        Nothing -> do 
            macceptHeader <- header "Accept" 
            return $ case macceptHeader of
                Just ag -> parseAcceptHeaders defaultFormat ag
                Nothing -> defaultFormat
  where
    -- possible usafety here!
    parseAcceptHeaders d
        = head 
        . map (parseFormat d . LT.strip . head . LT.splitOn ";") 
        . LT.splitOn "," 
    parseFormat d fmt = case fmt of
        "text/html" -> HTMLFormat
        "application/json" -> JSONFormat
        "text/plain" -> PlainTextFormat 
        "image/svg+xml" -> SVGFormat
        "*/*" -> d
        _ -> OtherFormat

uploadForm :: Html () 
uploadForm = 
    form_ [ enctype_ "multipart/form-data"
          , action_ "/traces"
          , method_ "POST"
          ]
          $ do
               "Trace file to upload:" 
               input_ [ name_ "tracetext", type_ "file"]
               input_ [ type_ "submit", value_ "Upload trace"]

textboxForm :: Html () 
textboxForm = 
    form_ [ enctype_ "multipart/form-data"
          , action_ "/traces"
          , method_ "POST"
          ]
          $ do
               "Trace text to upload:" 
               textarea_ [ name_ "tracetext", rows_ "10", cols_ "50", wrap_ "soft" ] $ return ()
               input_ [ type_ "submit", value_ "Upload trace"]

jsonPretty :: J.ToJSON a => a -> ActionM ()
jsonPretty j = do
    setHeader "Content-Type" "application/json; charset=utf-8" 
    raw $ encodePretty' (defConfig { confCompare = traceKeyOrder }) j  

traceUrl :: TraceId -> LT.Text
traceUrl traceId = LT.toLazyText $ "/traces/" <> LT.decimal traceId

nodeUrl :: TraceId -> NodeId -> LT.Text  
nodeUrl traceId nodeId = LT.toLazyText $ "/traces/" <> LT.decimal traceId <> "/nodes/" <> LT.decimal nodeId

server :: Int -> IO ()
server port = withSocketsDo $ do
    pages <- makePages 
    scotty port $ do
        get "/version" $ do
            rformat <- responseFormat JSONFormat
            case rformat of
                JSONFormat -> json $ M.singleton ("version"::LT.Text) version
                HTMLFormat -> html $ renderText $
                    html_ $ do
                        head_ (title_ "Version")
                        body_ $ toHtml version
                PlainTextFormat -> text $ version
                _ -> liftIO $ throwIO $ userError "unsupported Accept value"
        get "/" $ do
            addHeader "Location" "/traces"
            status status303 -- perform redirection 
        get "/traces" $ do
            rformat <- responseFormat JSONFormat
            (_, I.keys -> pm) <- liftIO $ readIORef pages
            let relurl i = LT.toLazyText $ "/traces/" <> LT.decimal i
            html $ renderText $
                html_ $ do
                    head_ $ title_ "List of traces"
                    body_ $ do
                        div_ $ do 
                            uploadForm
                            textboxForm
                        div_ $ do
                            T.forM pm $ \i -> do
                                a_ [href_ (LT.toStrict (relurl i))] (toHtml . show $ i)
                                " "
        post "/traces" $ do
            traceText <- traceUploadBody
            let result = Trace . numberTraceTree <$> parseIntoEither traceTreeParser traceText
            trace' <- either (liftIO . throwIO . userError) return result
            i <- liftIO $ atomicModifyIORef' pages (addPage (Parsed traceText trace'))
            let relurl = LT.toLazyText $ "/traces/" <> LT.decimal i
            addHeader "Location" relurl
            respf <- responseFormat JSONFormat
            case respf of
                JSONFormat -> do
                    status created201
                    json $ M.singleton ("url"::LT.Text) relurl  
                HTMLFormat -> do 
                    status status303 -- perform redirection 
                _ -> liftIO $ throwIO $ userError "unsupported Accept value"
        delete "/traces" $ do
            liftIO $ atomicModifyIORef' pages deleteAllPages
            status ok200
        get "/traces/:traceId" $ do
            traceId <- param "traceId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just t -> do
                    respf <- responseFormat JSONFormat
                    case respf of 
                        PlainTextFormat -> text $ _original t    
                        JSONFormat -> jsonPretty $ _parsedTrace t    
                        HTMLFormat -> html $ do  
                            let relurl = LT.toLazyText $ "/traces/" <> LT.decimal traceId 
                            renderText $
                                html_ $ do
                                    head_ (title_ "Trace")
                                    body_ $ do
                                        div_ $ do
                                            a_ [href_ (LT.toStrict $ relurl <> "?$format=text")] "text"
                                            " "
                                            a_ [href_ (LT.toStrict $ relurl <> "?$format=json")] "json"
                                            " (use "
                                            a_ [href_ jsonViewURL] "JSON View" 
                                            ")"
                        _ -> liftIO $ throwIO $ userError "unsupported Accept value" 
        delete "/traces/:traceId" $ do
            traceId <- param "traceId"
            liftIO $ atomicModifyIORef' pages (deletePage traceId)
            status ok200
        get "/traces/:traceId/nodes/:nodeId" $ do
            traceId <- param "traceId"
            nodeId <- param "nodeId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (_parsedTrace -> t) -> case IL.lookup nodeId (nodeMap t) of
                    Nothing ->  liftIO . throwIO $ userError "node does not exist"
                    Just t' -> do
                        respf <- responseFormat JSONFormat
                        case respf of 
                            JSONFormat -> jsonPretty $ t'    
                            HTMLFormat -> html $ do  
                                let relurl = LT.toLazyText $ "/traces/" <> LT.decimal traceId
                                let relurl2 = LT.toLazyText $ "/traces/" <> LT.decimal traceId <> "/nodes/" <> LT.decimal nodeId
                                renderText $
                                    html_ $ do
                                        head_ (title_ "Trace node")
                                        body_ $ do
                                            div_ $ do
                                                a_ [href_ (LT.toStrict $ relurl2 <> "?$format=json")] "json"
                                                " (use "
                                                a_ [href_ jsonViewURL] "JSON View" 
                                                ")"
                                            div_ $ do
                                                a_ [href_ (LT.toStrict $ relurl2 <> "/parents")] "list of parents"
                                            div_ $ do
                                                a_ [href_ (LT.toStrict $ relurl)] "back to whole trace"
                            _ -> liftIO $ throwIO $ userError "unsupported Accept value" 
        get "/traces/:traceId/nodes/:nodeId/parents" $ do
            traceId <- param "traceId"
            nodeId <- param "nodeId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (_parsedTrace -> t) -> case IL.lookup nodeId (nodeParentIdsMap t) of
                    Nothing ->  liftIO . throwIO $ userError "node does not exist"
                    Just t' -> do
                        let ids = map (nodeUrl traceId) t'
                        respf <- responseFormat JSONFormat
                        case respf of 
                            JSONFormat -> jsonPretty $ map (nodeUrl traceId) t'
                            HTMLFormat -> html $ renderText $ do
                                head_ (title_ "Trace node parents")
                                body_ $ do
                                   div_ $ do 
                                       T.forM t' $ \i -> 
                                           div_ $ do
                                                a_ [href_ (LT.toStrict (nodeUrl traceId i))] (toHtml . show $ i)
                            _ -> liftIO $ throwIO $ userError "unsupported Accept value" 

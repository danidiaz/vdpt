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
import qualified Data.Foldable as F
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
import Control.Comonad
import Control.Lens
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

renderDifferences :: TraceId -> TraceId -> [(NodeId, NodeId, [Difference])] -> Html () 
renderDifferences tid1 tid2 diffs = 
    ul_ $ do
        F.forM_ diffs $ \(nid1,nid2,nodeDiffs)  -> do
            let (nurl1,nurl2) = (nodeUrl tid1 nid1, nodeUrl tid2 nid2) 
            li_ $ do 
                div_ $ do
                    a_ [href_ (LT.toStrict nurl1)] (toHtml . LT.toLazyText $ LT.decimal nid1)
                    " "
                    a_ [href_ (LT.toStrict nurl2)] (toHtml . LT.toLazyText $ LT.decimal nid2)
                div_ $ 
                    ul_ $ do
                        F.forM_ nodeDiffs $ \d -> do
                            li_ $ case d of 
                                DifferentNumberOfChildren _ _ -> "Different number of children."
                                DifferentNodeTypes t1 t2 -> toHtml $ "Different node types: " <> t1 <> " vs. " <> t2 
                                AttributeDissapeared t -> toHtml $ "Attribute removed: " <> t
                                AttributeChanged t j1 j2 -> do
                                    div_ $ toHtml $ "Attribute changed: " <> t
                                    div_ $ do
                                        ul_ $ do
                                            li_ $ do
                                                pre_ $ toHtml . LT.toLazyText . encodePrettyToTextBuilder $ j1
                                            li_ $ do
                                                pre_ $ toHtml . LT.toLazyText . encodePrettyToTextBuilder $ j2
        
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
                            F.forM_ pm $ \i -> do
                                a_ [href_ (LT.toStrict (relurl i))] (toHtml . show $ i)
                                " "
        post "/traces" $ do
            traceText <- traceUploadBody
            let result = Trace . numberNodeTree <$> parseIntoEither traceTreeParser traceText
            trace' <- either (liftIO . throwIO . userError) return result
            i <- liftIO $ atomicModifyIORef' pages (addPage (Parsed traceText trace'))
            let relurl = LT.toLazyText $ "/traces/" <> LT.decimal i
            addHeader "Location" relurl
            respf <- responseFormat JSONFormat
            case respf of
                JSONFormat -> do
                    status created201
                    json $ M.singleton ("url"::LT.Text) (J.toJSON relurl) <> M.singleton "id" (J.toJSON i)
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
                                        div_ $ a_ [href_ $ LT.toStrict $ relurl <> "/analyses"] "analyses"
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
                Just (getTrace . _parsedTrace -> t) -> case IL.lookup nodeId (nodeMap t) of
                    Nothing ->  liftIO . throwIO $ userError "node does not exist"
                    Just t' -> do
                        respf <- responseFormat JSONFormat
                        case respf of 
                            JSONFormat -> jsonPretty $ Trace t'    
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
                                                a_ [href_ (LT.toStrict $ relurl2 <> "/ancestors")] "ancestors"
                                            div_ $ do
                                                a_ [href_ (LT.toStrict $ relurl)] "back to whole trace"
                            _ -> liftIO $ throwIO $ userError "unsupported Accept value" 
        get "/traces/:traceId/nodes/:nodeId/ancestors" $ do
            traceId <- param "traceId"
            nodeId <- param "nodeId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (getTrace . _parsedTrace -> t) -> case IL.lookup nodeId (nodeParentsMap t) of
                    Nothing ->  liftIO . throwIO $ userError "node does not exist"
                    Just t' -> do
                        -- let ids = map (nodeUrl traceId) t'
                        let parents = over (mapped._2) (nodeUrl traceId . _nodeId . extract) t'
                            parentswtype = over (mapped._2) ((,,) <$> _nodeId . extract <*> nodeUrl traceId . _nodeId  . extract <*> _nodeType . extract) t'
                        respf <- responseFormat JSONFormat
                        case respf of 
                            JSONFormat -> jsonPretty $ parents
                            HTMLFormat -> html $ renderText $ do
                                head_ (title_ "Trace node ancestors")
                                body_ $ do
                                   div_ $ do 
                                       T.forM parentswtype $ \(ind, (pId, pUrl,pType)) -> 
                                           div_ $ do
                                                a_ [href_ (LT.toStrict pUrl)] (toHtml . show $ pId)
                                                " #" 
                                                (toHtml . show $ ind)   
                                                " "
                                                (toHtml pType)   
                            _ -> liftIO $ throwIO $ userError "unsupported Accept value" 
        get "/traces/:traceId/differences/:traceId2" $ do
            traceId <- param "traceId"
            traceId2 <- param "traceId2"
            (_,m) <- liftIO $ readIORef pages 
            case (,) <$> I.lookup traceId m <*> I.lookup traceId2 m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (getTrace . _parsedTrace -> t1, getTrace . _parsedTrace -> t2) -> do
                    let theDiffs = nodeTreeDiff t1 t2 
                    respf <- responseFormat JSONFormat
                    case respf of 
                        JSONFormat -> 
                              jsonPretty  
                            . over (mapped._1) (nodeUrl traceId) 
                            . over (mapped._2) (nodeUrl traceId2) 
                            $ theDiffs
                        HTMLFormat -> html $ renderText $ do
                            head_ (title_ "Differences")
                            body_ $ do
                               div_ $ do
                                   let relurl = LT.toLazyText $ "/traces/" <> 
                                                LT.decimal traceId <> "/differences/" <> LT.decimal traceId2 
                                   a_ [href_ (LT.toStrict $ relurl <> "/?$format=json")] "json"
                                   " (use "
                                   a_ [href_ jsonViewURL] "JSON View" 
                                   ")"
                               div_ $ do 
                                   renderDifferences traceId traceId2 theDiffs   
                        _ -> liftIO $ throwIO $ userError "unsupported Accept value"
        get "/traces/:traceId/analyses" $ do
            traceId <- param "traceId"
            let url = traceUrl traceId
            html $ renderText $ do
                head_ (title_ "Available analyses")
                body_ $ do
                   div_ $ a_ [href_ $ LT.toStrict $ url <> "/analyses/nodecountsbytype"] "node counts by type"
                   div_ $ a_ [href_ $ LT.toStrict $ url <> "/analyses/directancestorsbytype"] "direct ancestors by type"
        get "/traces/:traceId/analyses/nodecountsbytype" $ do
            traceId <- param "traceId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (nodeTypeCounts . getTrace . _parsedTrace -> t) -> jsonPretty t
        get "/traces/:traceId/analyses/directancestorsbytype" $ do
            traceId <- param "traceId"
            (_,m) <- liftIO $ readIORef pages 
            case I.lookup traceId m of
                Nothing -> liftIO . throwIO $ userError "trace does not exist"
                Just (directAncestorsByType . getTrace . _parsedTrace -> t) -> jsonPretty t

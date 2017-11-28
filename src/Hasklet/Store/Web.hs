{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hasklet.Store.Web(startServer) where

import           Hasklet.Store.Transform
import           Hasklet.Store.Types

import           Control.Exception          hiding (Handler)
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy       as B
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4
import           Network.Wai.Handler.Warp
import           Servant                    hiding (contentType)

type ContentAPI = "content" :> ReqBody '[JSON] NewContent
                            :> Post '[JSON] Content
             :<|> "content" :> Capture "contentId" UUID
                            :> ReqBody '[JSON] NewContent
                            :> Put '[JSON] Content
             :<|> "content" :> Capture "contentId" UUID
                            :> Get '[JSON] Content
             :<|> "content" :> QueryParam "limit" Int
                            :> QueryParam "continue" UUID
                            :> QueryParam "type" T.Text
                            :> QueryParam "time" UTCTime
                            :> Get '[JSON] [Content]
 {-            :<|> "content" :> Capture "contentId" UUID :> ReqBody '[JSON] NewContent
                                                        :> DELETE
             :<|> "content" :> Capture "contentId" UUID :> "fields" :> ReqBody '[JSON] NewContent
                                                                    :> PATCH
-}

startServer :: Port -> DatabaseActions conn -> IO ()
startServer p da = run p (serve contentAPIProxy $ server da)

contentAPIProxy :: Proxy ContentAPI
contentAPIProxy = Proxy

server :: DatabaseActions conn -> Server ContentAPI
server da = enter (handlerTransform da) (
    postContentHandler :<|>
    putContentHandler :<|>
    getContentHandler :<|>
    getAllContentHandler)

handlerTransform :: DatabaseActions conn -> StoreHandler conn :~> Handler
handlerTransform da = NT $ runStoreHandler da

runStoreHandler :: DatabaseActions conn -> StoreHandler conn a -> Handler a
runStoreHandler da s = Handler $ ExceptT (transactionalHandler `catch` (pure . Left)) where
    transactionalHandler = withTransaction da $ \conn -> do
        let ctx = HandlerContext conn da
            inner = runReaderT s ctx
        res <- runExceptT inner
        case res of
            Left e  -> throwIO e -- throw error here to abort transaction
            Right r -> pure (Right r)

putContentHandler :: UUID -> NewContent -> StoreHandler conn Content
putContentHandler cId new@(NewContent newT newFs) = do
    existing <- query getContent contentQuery{ whereId = Just cId }
    case existing of
        []       -> insertNewPost (Just cId) new
        (_:_:_) -> throwStoreError err500
        [Content { contentType = oldT, fields = oldFs }] ->
            do when (oldT /= newT) $ throwStoreError err400 { errBody = B.concat [
                   "Cannot change content type. Server content type is '",
                   B.fromStrict $ encodeUtf8 oldT,
                   "' but attempting to put a content type of '",
                   B.fromStrict $ encodeUtf8 newT,
                   "'" ]}
               case fieldChanges oldFs newFs of
                    Left err -> throwStoreError $ err400 { errBody = err }
                    Right [] -> pure ()
                    Right cs -> liftIO getCurrentTime >>= \time ->
                              query insertFields (cId, time, cs)
               getContentHandler cId


postContentHandler :: NewContent -> StoreHandler conn Content
postContentHandler = insertNewPost Nothing

getAllContentHandler :: Maybe Int -> Maybe UUID -> Maybe T.Text -> Maybe UTCTime -> StoreHandler conn [Content]
getAllContentHandler limit continue cType time = query getContent params where
    params = contentQuery{ whereLimit=limit,
                           whereContinueId=continue,
                           whereType=cType,
                           whereTime=time }

getContentHandler :: UUID -> StoreHandler conn Content
getContentHandler cId = do content <- query getContent contentQuery{ whereId = Just cId }
                           case content of
                               []  -> throwStoreError err404
                               [c] -> pure c
                               _   -> throwStoreError err500

insertNewPost :: Maybe UUID -> NewContent -> StoreHandler conn Content
insertNewPost mId (NewContent cType fs) = do
    cId <- case mId of
            Nothing  -> liftIO nextRandom
            Just cId -> pure cId
    time <- liftIO getCurrentTime
    query insertContent (cId, cType, time)
    case fromJSON fs of
        Left err  -> throwStoreError $ err400 { errBody = err }
        Right kvp -> query insertFields (cId, time, kvp)
    getContentHandler cId

-- | Pass a DatabaseActions getter and a request object and run the request in the StoreHandler Monad
query :: (DatabaseActions conn -> conn -> req -> IO res) -> req -> StoreHandler conn res
query getAction req = do (HandlerContext conn as) <- ask
                         let dbAction = getAction as
                         liftIO $ dbAction conn req

throwStoreError :: ServantErr -> StoreHandler conn a
throwStoreError = lift . throwError

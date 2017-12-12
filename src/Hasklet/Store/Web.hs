{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hasklet.Store.Web(application) where

import           Hasklet.Store.Transform
import           Hasklet.Store.Types

import           Control.Exception          hiding (Handler)
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4
import           Servant                    hiding (contentType)

type ContentAPI = "content" :> ReqBody '[JSON] NewContent
                            :> Post '[JSON] Content
             :<|> "content" :> Capture "contentId" UUID
                            :> ReqBody '[JSON] NewContent
                            :> Put '[JSON] Content
             :<|> "content" :> Capture "contentId" UUID
                            :> QueryParam "time" UTCTime
                            :> Get '[JSON] Content
             :<|> "content" :> QueryParam "limit" Int
                            :> QueryParam "continue" UUID
                            :> QueryParam "type" T.Text
                            :> QueryParam "active" Bool
                            :> QueryParam "time" UTCTime
                            :> Get '[JSON] [Content]
             :<|> "content" :> Capture "contentId" UUID :> "versions"
                                                        :> Get '[JSON] [UTCTime]

application :: DatabaseActions conn -> Application
application da = serve contentAPIProxy $ server da

contentAPIProxy :: Proxy ContentAPI
contentAPIProxy = Proxy

server :: DatabaseActions conn -> Server ContentAPI
server da = enter (handlerTransform da) (
    postContentHandler :<|>
    putContentHandler :<|>
    getContentHandler :<|>
    getAllContentHandler :<|>
    getVersionsHandler)

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
putContentHandler cId new@(NewContent newT newA newFs) = do
    existing <- query getContent contentQuery{ whereId = Just cId }
    case existing of
        []      -> insertNewPost (Just cId) new
        (_:_:_) -> throwStoreError err500
        [Content { contentType = oldT, active = oldA, fields = oldFs }] -> do
               time <- liftIO getCurrentTime
               fChanged <- case fieldChanges oldFs newFs of
                             Left err -> throwStoreError $ err400 { errBody = err }
                             Right [] -> pure False
                             Right cs -> query insertFields (cId, time, cs) *> pure True
               let propChanged = oldT /= newT || oldA /= newA
               when propChanged $ query insertProperties (cId, time, newT, newA)
               when (propChanged || fChanged) $ query updateLastModified (cId, time)
               getContentHandler cId Nothing


postContentHandler :: NewContent -> StoreHandler conn Content
postContentHandler = insertNewPost Nothing

getAllContentHandler :: Maybe Int -> Maybe UUID -> Maybe T.Text -> Maybe Bool -> Maybe UTCTime -> StoreHandler conn [Content]
getAllContentHandler limit continue cType act time = query getContent params where
    params = contentQuery{ whereLimit=limit,
                           whereContinueId=continue,
                           whereActive=act,
                           whereType=cType,
                           whereTime=time }

getContentHandler :: UUID -> Maybe UTCTime -> StoreHandler conn Content
getContentHandler cId time =
    do content <- query getContent contentQuery{ whereId = Just cId, whereTime = time }
       case content of
           []  -> throwStoreError err404
           [c] -> pure c
           _   -> throwStoreError err500

getVersionsHandler :: UUID -> StoreHandler conn [UTCTime]
getVersionsHandler cId = getContentHandler cId Nothing *> query getVersions cId

insertNewPost :: Maybe UUID -> NewContent -> StoreHandler conn Content
insertNewPost mId (NewContent cType act fs) = do
    cId <- case mId of
            Nothing  -> liftIO nextRandom
            Just cId -> pure cId
    time <- liftIO getCurrentTime
    query insertContent (cId, time)
    query insertProperties (cId, time, cType, act)
    case fromJSON fs of
        Left err  -> throwStoreError $ err400 { errBody = err }
        Right kvp -> query insertFields (cId, time, kvp)
    getContentHandler cId Nothing

-- | Pass a DatabaseActions getter and a request object and run the request in the StoreHandler Monad
query :: (DatabaseActions conn -> conn -> req -> IO res) -> req -> StoreHandler conn res
query getAction req = do (HandlerContext conn as) <- ask
                         let dbAction = getAction as
                         liftIO $ dbAction conn req

throwStoreError :: ServantErr -> StoreHandler conn a
throwStoreError = lift . throwError

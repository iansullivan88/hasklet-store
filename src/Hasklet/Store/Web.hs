{-# LANGUAGE DataKinds             #-}
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
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4
import           Network.Wai.Handler.Warp
import           Servant

type ContentAPI = "content" :> ReqBody '[JSON] NewContent
                            :> Post '[JSON] Content
             :<|> "content" :> QueryParam "limit" Int
                            :> QueryParam "continue" UUID
                            :> QueryParam "type" T.Text
                            :> QueryParam "time" UTCTime
                            :> Get '[JSON] [Content]
             :<|> "content" :> Capture "contentId" UUID :> Get '[JSON] Content
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
server da = enter (handlerTransform da) (postContentHandler :<|> getAllContentHandler :<|> getContentHandler)

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

postContentHandler :: NewContent -> StoreHandler conn Content
postContentHandler (NewContent cType fs) = do
    cId <- liftIO nextRandom
    time <- liftIO getCurrentTime
    query insertContent (cId, cType, time)
    case fromJSON fs of
        Left err  -> throwStoreError $ err400 { errBody = err }
        Right kvp -> query insertFields (cId, time, kvp)
    getContentHandler cId

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

-- | Pass a DatabaseActions getter and a request object and run the request in the StoreHandler Monad
query :: (DatabaseActions conn -> conn -> req -> IO res) -> req -> StoreHandler conn res
query getAction req = do (HandlerContext conn as) <- ask
                         let dbAction = getAction as
                         liftIO $ dbAction conn req

throwStoreError :: ServantErr -> StoreHandler conn a
throwStoreError = lift . throwError

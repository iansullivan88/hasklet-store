{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Hasklet.Store.Web where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Pool
import qualified Data.Text                  as T
import           Data.UUID
import           Hasklet.Store.Types
import           Servant

type ContentAPI = "content" :> ReqBody '[JSON] NewContent
                            :> Post '[JSON] Content
             :<|> "content" :> QueryParam "limit" Int
                            :> QueryParam "skip" Int
                            :> QueryParam "type" T.Text
                            :> Get '[JSON] [Content]
             :<|> "content" :> Capture "contentId" UUID :> Get '[JSON] Content
             :<|> "content" :> Capture "contentId" UUID :> ReqBody '[JSON] NewContent
                                                        :> DELETE
             :<|> "content" :> Capture "contentId" UUID :> "fields" :> ReqBody '[JSON] NewContent
                                                                    :> PATCH

server :: Server ContentAPI
server = postContentHandler :<|> getAllContentHandler :<|> error "todo"

runStoreHandler :: DatabaseActions conn -> StoreHandler conn a -> Handler a
runStoreHandler da s = do h <- liftIO $ withTransaction da (\conn ->
                                    let ctx = HandlerContext conn da
                                    in  pure $ runReaderT s ctx)
                          Handler h

postContentHandler :: NewContent -> Handler Content
postContentHandler = error "todo"

getAllContentHandler :: Maybe Int -> Maybe Int -> Maybe T.Text -> Handler [Content]
getAllContentHandler = error "todo"

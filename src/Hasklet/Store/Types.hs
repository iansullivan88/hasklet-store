{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasklet.Store.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.UUID
import           Servant.Server

data FieldValue = NoField
                | NullField
                | NumberField Double
                | TextField T.Text
                | BoolField Bool deriving(Eq)

data Content = Content {
    id               :: UUID,
    contentType      :: T.Text,
    active           :: Bool,
    lastModifiedTime :: UTCTime,
    createdTime      :: UTCTime,
    fields           :: Value
} deriving(Show)
$(deriveJSON defaultOptions ''Content)

data NewContent = NewContent {
    contentType :: T.Text,
    active      :: Bool,
    fields      :: Value
} deriving(Show)
$(deriveJSON defaultOptions ''NewContent)

data HandlerContext conn = HandlerContext {
    connection      :: conn,
    databaseActions :: DatabaseActions conn
}

type StoreHandler conn = ReaderT (HandlerContext conn) (ExceptT ServantErr IO)

data ContentQuery = ContentQuery {
    whereId         :: Maybe UUID,
    whereType       :: Maybe T.Text,
    whereActive     :: Maybe Bool,
    whereContinueId :: Maybe UUID,
    whereTime       :: Maybe UTCTime,
    whereLimit      :: Maybe Int
}

data DatabaseActions conn = DatabaseActions {
    withTransaction     :: forall a. (conn -> IO a) -> IO a,
    insertContent       :: conn -> (UUID, UTCTime) -> IO (),
    insertFields        :: conn -> (UUID, UTCTime, [(T.Text, FieldValue)]) -> IO (),
    insertProperties    :: conn -> (UUID, UTCTime, T.Text, Bool) -> IO (),
    updateLastModified  :: conn -> (UUID, UTCTime) -> IO (),
    getContent          :: conn -> ContentQuery -> IO [Content],
    getVersions         :: conn -> UUID -> IO [UTCTime]
}

-- | Default 'ContentQuery'
contentQuery :: ContentQuery
contentQuery = ContentQuery Nothing Nothing Nothing Nothing Nothing Nothing

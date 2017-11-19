{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasklet.Store.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy          as H
import           Data.Scientific
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.UUID
import           Servant.Server

data FieldValue = NullField | DoubleField Double | TextField T.Text | BoolField Bool

instance ToJSON FieldValue where
    toJSON (DoubleField d) = Number $ fromFloatDigits d
    toJSON (TextField t)   = String t
    toJSON (BoolField b)   = Bool b
    toJSON NullField       = Null

instance FromJSON FieldValue where
    parseJSON (Number d) = return $ DoubleField $ toRealFloat d
    parseJSON (String t) = return $ TextField t
    parseJSON (Bool b)   = return $ BoolField b
    parseJSON Null       = return NullField
    parseJSON invalid    = typeMismatch "FieldValue" invalid

type Fields = H.HashMap T.Text FieldValue

data Content = Content {
    id               :: UUID,
    contentType      :: T.Text,
    active           :: Bool,
    lastModifiedTime :: UTCTime,
    createdTime      :: UTCTime,
    fields           :: Fields
}
$(deriveJSON defaultOptions ''Content)

data NewContent = NewContent {
    newContentType :: T.Text,
    newFields      :: Fields
}
$(deriveJSON defaultOptions ''NewContent)

data HandlerContext conn = HandlerContext {
    connection      :: conn,
    databaseActions :: DatabaseActions conn
}

type StoreHandler conn = ReaderT (HandlerContext conn) (ExceptT ServantErr IO)

data ContentQuery = ContentQuery {
    whereId         :: Maybe UUID,
    whereType       :: Maybe T.Text,
    whereContinueId :: Maybe UUID,
    whereTime       :: Maybe UTCTime,
    whereLimit      :: Maybe Int
}

contentQuery :: ContentQuery
contentQuery = ContentQuery Nothing Nothing Nothing Nothing Nothing

data DatabaseActions conn = DatabaseActions {
    withTransaction :: forall a. (conn -> IO a) -> IO a,
    insertContent   :: conn -> (UUID, T.Text, UTCTime) -> IO (),
    insertFields    :: conn -> (UUID, UTCTime, [(T.Text, FieldValue)]) -> IO (),
    queryContent    :: conn -> ContentQuery -> IO [Content]
}

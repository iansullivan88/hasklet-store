{-# LANGUAGE RankNTypes #-}

module Hasklet.Store.Types where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy          as H
import           Data.Scientific
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Data.UUID
import           Database.SQLite.Simple
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

data NewContent = NewContent {
    newContentType :: T.Text,
    newFields      :: Fields
}

data FieldRow = FieldRow {
    contentId :: UUID,
    key       :: T.Text,
    version   :: UTCTime,
    value     :: FieldValue
}

data HandlerContext conn = HandlerContext {
    connection      :: conn,
    databaseActions :: DatabaseActions conn
}

type StoreHandler conn = ReaderT (HandlerContext conn) (ExceptT ServantErr IO)

data DatabaseActions conn = DatabaseActions {
    withTransaction :: forall a. (conn -> IO a) -> IO a,
    insertContent   :: conn -> (UUID, T.Text, [FieldRow]) -> IO ()
}

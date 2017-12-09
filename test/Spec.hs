{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Hasklet.Store.Database
import           Hasklet.Store.Types
import           Hasklet.Store.Web

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.UUID
import qualified Data.Vector               as V
import           Network.HTTP.Types.Method
import           Network.Wai               (Application)
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.HUnit.Base
import           Web.Internal.HttpApiData

main :: IO ()
main = hspec $ with setupApp $ do
    describe "GET /content" $ do
        it "responds with empty list if there is no content" $
            get "/content" `shouldRespondWith` "[]"
        it "responds with all content" $ do
            void $ postContent newContent1
            void $ postContent newContent2
            get "/content" `jsonResponse` (`shouldSatisfy` \cs -> matchesAllNewContent [newContent1, newContent2] cs ||
                                                                  matchesAllNewContent [newContent2, newContent1] cs)
        it "can get content at a specific time" $ do
            Content {id = _id} <- postContent newContent1
            time <- liftIO getCurrentTime
            wait
            putJSON ("/content/" `B.append` toASCIIBytes _id) (encode newContent2) `shouldRespondWith` 200
            void $ postContent newContent2
            get ("/content?time=" `B.append` byteStringQueryParam time) `jsonResponse` (`shouldSatisfy` matchesAllNewContent [newContent1])

    describe "GET /content/:id" $
        it "retrieves content" $ do
            Content {id = id1} <- postContent newContent1
            Content {id = id2} <- postContent newContent2
            get ("/content/" `B.append` toASCIIBytes id1) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent1)
            get ("/content/" `B.append` toASCIIBytes id2) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent2)
    describe "POST /content" $ do
        it "responds with content" $
            postJSON "/content" (encode newContent1) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent1)
        it "doesn't allow JSON arrays" $
            postJSON "/content" (encode $ NewContent "type1" True (Array V.empty)) `shouldRespondWith` 400
    describe "PUT /content/:id" $ do
        it "replaces content" $ do
            Content {id = _id} <- postContent newContent1
            putJSON ("/content/" `B.append` toASCIIBytes _id) (encode newContent2) `shouldRespondWith` 200
            get "/content/" `jsonResponse` (`shouldSatisfy` matchesAllNewContent [newContent2])
        it "updates last modified time when there are changes" $ do
            Content {id = _id, lastModifiedTime = lmt, createdTime = ct} <- postContent newContent1
            wait
            putJSON ("/content/" `B.append` toASCIIBytes _id) (encode newContent2) `shouldRespondWith` 200
            get ("/content/" `B.append` toASCIIBytes _id) `jsonResponse` (`shouldSatisfy`
                \Content { id = _id', lastModifiedTime = lmt', createdTime = ct' } -> _id == _id' && lmt' > lmt && ct == ct')
        it "doesn't change the modified time when there are no changes" $ do
            Content {id = _id, lastModifiedTime = lmt, createdTime = ct} <- postContent newContent1
            wait
            putJSON ("/content/" `B.append` toASCIIBytes _id) (encode newContent1) `shouldRespondWith` 200
            get ("/content/" `B.append` toASCIIBytes _id) `jsonResponse` (`shouldSatisfy`
                \Content { id = _id', lastModifiedTime = lmt', createdTime = ct' } -> _id == _id' && lmt' == lmt && ct == ct') where
    wait = liftIO $ threadDelay 100000
    byteStringQueryParam = encodeUtf8 . toQueryParam




postContent :: NewContent -> WaiSession Content
postContent nc = do
    res <- postJSON "/content" (encode nc)
    case eitherDecode' (simpleBody res) of
        Left e  -> liftIO $ assertFailure' ("Could not decode content: " ++ e)
        Right c -> pure c

newContent1 :: NewContent
newContent1 = NewContent "type1" True $ object [
    "x" .= (1 :: Int),
    "y" .= ("2" :: String),
    "z" .= object [ "nested" .= True]]


newContent2 :: NewContent
newContent2 = NewContent "type2" False $ object [
    "a" .= Null,
    "b" .= (2 :: Int),
    "c" .= (3 :: Int)]

jsonResponse :: (HasCallStack, FromJSON a) => WaiSession SResponse -> (a -> Expectation) -> WaiSession ()
jsonResponse rAct expectation = do r <- rAct
                                   shouldRespondWith rAct 200
                                   liftIO $ case eitherDecode' (simpleBody r) of
                                       Left e  -> assertFailure' e
                                       Right b -> expectation b

matchesAllNewContent :: [NewContent] -> [Content] -> Bool
matchesAllNewContent ncs cs = length ncs == length cs && and (zipWith matchesNewContent ncs cs)

matchesNewContent :: NewContent -> Content -> Bool
matchesNewContent (NewContent t a fs) (Content _ t' a' _ _ fs') = t == t' && a == a' && fs == fs'

setupApp :: IO Application
setupApp = do
    da <- createDatabaseActions ":memory:" 1
    withTransaction da createSchema
    pure $ application da

postJSON :: B.ByteString -> LB.ByteString -> WaiSession SResponse
postJSON urlPath = Test.Hspec.Wai.request methodPost urlPath [("Content-Type","application/json")]

putJSON :: B.ByteString -> LB.ByteString -> WaiSession SResponse
putJSON urlPath = Test.Hspec.Wai.request methodPut urlPath [("Content-Type","application/json")]

assertFailure' :: HasCallStack => String -> IO a
assertFailure' e = assertFailure e >> error "should never be reached"

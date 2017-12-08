{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Hasklet.Store.Database
import           Hasklet.Store.Types
import           Hasklet.Store.Web

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import           Data.UUID
import           Network.HTTP.Types.Method
import           Network.Wai               (Application)
import           Network.Wai.Test
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.HUnit.Base

main :: IO ()
main = hspec $ with setupApp $ do
    describe "GET /content" $ do
        it "responds with empty list" $
            get "/content" `shouldRespondWith` "[]"
        it "responds with content" $ do
            void $ postContent newContent1
            void $ postContent newContent2
            get "/content" `jsonResponse` (`shouldSatisfy` \cs -> matchesAllNewContent [newContent1, newContent2] cs ||
                                                                  matchesAllNewContent [newContent2, newContent1] cs)
    describe "GET /content/:id" $
        it "responds with content" $ do
            Content {id = id1} <- postContent newContent1
            Content {id = id2} <- postContent newContent2
            get ("/content/" `B.append` toASCIIBytes id1) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent1)
            get ("/content/" `B.append` toASCIIBytes id2) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent2)
    describe "POST /content" $
        it "responds with content" $
            postJSON "/content" (encode newContent1) `jsonResponse` (`shouldSatisfy` matchesNewContent newContent1)

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
newContent2 = NewContent "type2" True $ object [
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

assertFailure' :: HasCallStack => String -> IO a
assertFailure' e = assertFailure e >> error "should never be reached"

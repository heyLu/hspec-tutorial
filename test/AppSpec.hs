{-# LANGUAGE OverloadedStrings #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Network.Wai.Test (SResponse)

import           Data.String
import           Data.ByteString (ByteString)
import           Control.Applicative

import           Helper
import           App

main :: IO ()
main = hspec spec

data InvalidPath = InvalidPath ByteString
  deriving Show

instance Arbitrary InvalidPath where
  arbitrary = InvalidPath . fromString <$> (listOf1 . elements) ['A'..'z']

get :: ByteString -> IO SResponse
get path = app >>= getPath path

spec :: Spec
spec = do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      get "/" `shouldRespondWith` 200

    it "returns the service name" $ do
      r <- get "/"
      body r `shouldContain` "\"name\":\"time-service\""

    it "returns the service version" $ do
      r <- get "/"
      body r `shouldContain` "\"version\":\"0.1.0\""

  context "when given an invalid request path" $ do
    it "responds with HTTP status 404" $ do
      get "/some-path" `shouldRespondWith` 404

  context "when given an *arbitrary* invalid request path" $ do
    it "responds with HTTP status 404" $ do
      property $ \(InvalidPath path) -> do
        get path `shouldRespondWith` 404

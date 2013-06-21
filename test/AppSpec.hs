{-# LANGUAGE OverloadedStrings #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Network.Wai.Test (SResponse)
import           Data.ByteString (ByteString, pack)
import           Control.Applicative

import           Helper
import           App

main :: IO ()
main = hspec spec

get :: ByteString -> IO SResponse
get path = app >>= getPath path

instance Arbitrary ByteString where
  arbitrary = fmap pack arbitrary

spec :: Spec
spec = do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      get "/" `shouldRespondWith` 200

    it "says 'Hello!'" $ do
      (body <$> get "/") `shouldReturn` "{\"body\":\"Hello!\"}"

  context "when given an invalid request path" $ do
    it "responds with HTTP status 404" $ do
      get "/invalid" `shouldRespondWith` 404

  context "when given an *arbitrary* invalid request path" $ do
    it "responds with HTTP status 404" $ do
      property $ \s -> s `notElem` ["", "/"] ==>
        get s `shouldRespondWith` 404

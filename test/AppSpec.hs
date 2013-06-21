{-# LANGUAGE OverloadedStrings #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Network.Wai.Test (SResponse)
import qualified Data.ByteString as B
import           Control.Applicative

import           Helper
import           App

main :: IO ()
main = hspec spec

get :: B.ByteString -> IO SResponse
get path = app >>= getPath path

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary `suchThat` \s ->
                    s `notElem` ["", "/", "/oops"] && (B.take 1 s /= "?")

spec :: Spec
spec = do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      (statusCode <$> get "/") `shouldReturn` 200

    it "says 'Hello!'" $ do
      (body <$> get "/") `shouldReturn` "{\"body\":\"Hello!\"}"

  context "when given an invalid request path" $ do
    it "responds with HTTP status 404" $ do
      (statusCode <$> get "/invalid") `shouldReturn` 404

  context "when given an *arbitrary* invalid request path" $ do
    it "responds with HTTP status 404" $ do
      property $ \s ->
        (statusCode <$> get s) `shouldReturn` 404

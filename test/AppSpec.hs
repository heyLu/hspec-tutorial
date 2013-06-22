{-# LANGUAGE OverloadedStrings #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Network.Wai.Test (SResponse)
import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Data.Time

import           Helper
import           App hiding (body)

main :: IO ()
main = hspec spec

get :: B.ByteString -> IO SResponse
get path = getCurrentTime >>= app >>= getPath path

getWithTime :: UTCTime -> B.ByteString -> IO SResponse
getWithTime time path = app time >>= getPath path

newtype Path = Path B.ByteString deriving Show
instance Arbitrary Path where
    arbitrary = fmap (Path . B.pack) $ listOf . oneof $ [choose ('a', 'z'), choose ('0', '9'), return '/']

isNoPathIn p ps = normalizePath p `notElem` allowedPaths
    where allowedPaths = concatMap (\path -> [path, B.snoc path '/']) ps
normalizePath = fst . B.foldl step ("", False)
    where step (p, True)  '/' = (p, True)
          step (p, False) '/' = (B.snoc p '/', True)
          step (p, _)     c   = (B.snoc p c, False)

spec :: Spec
spec = do
  describe "GET /" $ do
    it "responds with HTTP status 200" $ do
      get "/" `shouldRespondWith` 200

    it "responds with name" $ do
      body <- body <$> get "/"
      body `shouldContain` "version"
      body `shouldContain` "0.1.0"

    it "responds with version" $ do
      body <- body <$> get "/"
      body `shouldContain` "name"
      body `shouldContain` "time-service"

  describe "GET /current_time" $ do
    it "responds with HTTP status 200" $ do
      get "/current_time" `shouldRespondWith` 200

  describe "GET /current_time" $ do
    it "contains a current_time field" $ do
      let t = UTCTime (fromGregorian 2013 6 22)
                      (secondsToDiffTime $ 19*60*60 + 9*60 + 13)
      body <- body <$> getWithTime t "/current_time"
      body `shouldContain` "current_time"
      body `shouldContain` "2013-06-22 19:09:13 UTC"

  describe "GET /hello" $ do
    it "says 'Hello!'" $ do
      (body <$> get "/hello") `shouldReturn` "{\"body\":\"Hello!\"}"

  context "when given an invalid request path" $ do
    it "responds with HTTP status 404" $ do
      get "/invalid" `shouldRespondWith` 404

  context "when given an *arbitrary* invalid request path" $ do
    it "responds with HTTP status 404" $ do
      property $ \(Path p) -> p `isNoPathIn` ["", "/current_time", "/hello"] ==>
        get (p) `shouldRespondWith` 404

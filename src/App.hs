{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module App where

import           Data.Data
import           Data.JSON (toJSON)
import           Data.Conduit
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.Conduit.List (sourceList)
import           Web.Scotty hiding (json)
import           Network.Wai
import           Data.Time
import           Control.Monad.IO.Class (liftIO)

data Service = Service {
  serviceName    :: String
, serviceVersion :: String
} deriving (Eq, Show, Data, Typeable)

data Response = Response {
  currentTime :: String
} deriving (Eq, Show, Data, Typeable)

json :: Data a => a -> ActionM ()
json data_ = do
  header "Content-Type" "application/json"
  (source . sourceList . return . Chunk . fromByteString . toJSON) data_

app :: IO UTCTime -> IO Application
app getTime = scottyApp $ do
  get "/" $ do
    json (Service "time-service" "0.1.0")

  get "/current-time.json" $ do
    t <- liftIO getTime
    json (Response $ show t)

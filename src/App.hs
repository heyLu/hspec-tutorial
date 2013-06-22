{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module App where

import           Data.Aeson hiding (json)
import           GHC.Generics
import           Data.Conduit
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.Conduit.List (sourceList)
import           Web.Scotty hiding (body)
import           Network.Wai

data Message = Message {
  body :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

app :: IO Application
app = scottyApp $ do
  get "/" $ do
    json (Message "Hello!")

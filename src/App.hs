{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module App where

import           Data.Aeson hiding (json)
import           GHC.Generics
import           Web.Scotty hiding (body)
import           Network.Wai
import           Text.Read (readMaybe)
import qualified Data.Version as V

data Message = Message {
  body :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

data Version = Version {
    name :: String,
    version :: V.Version
}

parseVersion str = maybe v0 id (readMaybe str)
    where v0 = V.Version [0,0,0] []

instance ToJSON Version where
    toJSON (Version name version) =
        object [
            "name" .= name,
            "version" .= V.showVersion version
        ]

app :: IO Application
app = scottyApp $ do
  get "" $ json (Version "time-service" (V.Version [0, 1, 0] []))
  get "/hello" $ do
    json (Message "Hello!")

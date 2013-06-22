{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module App where

import           Data.Aeson hiding (json)
import           GHC.Generics
import           Web.Scotty hiding (body)
import           Network.Wai
import           Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.List.Split (splitWhen)

data Message = Message {
  body :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

data Version = Version {
    name :: String,
    version :: (Int, Int, Int)
}

showVersion (maj, min, patch) =
    intercalate "." $ map show [maj, min, patch]

parseVersion str = (maj, min, patch)
  where [maj, min, patch] = take 3 parts
        parts = parsed ++ repeat 0
        parsed = mapMaybe readMaybe $ splitWhen (== '.') str

instance ToJSON Version where
    toJSON (Version name version) =
        object [
            "name" .= name,
            "version" .= showVersion version
        ]

app :: IO Application
app = scottyApp $ do
  get "" $ json (Version "time-service" (0, 1, 0))
  get "/hello" $ do
    json (Message "Hello!")

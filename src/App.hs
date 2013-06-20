{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module App where

import           Data.Data
import           Data.JSON (toJSON)
import           Data.Conduit
import           Blaze.ByteString.Builder.ByteString (fromByteString)
import           Data.Conduit.List (sourceList)
import           Web.Scotty hiding (json)
import           Network.Wai
import qualified Data.Map as M

data Message = Message {
  messageBody :: String
} deriving (Eq, Show, Data, Typeable)

json :: Data a => a -> ActionM ()
json data_ = do
  header "Content-Type" "application/json"
  (source . sourceList . return . Chunk . fromByteString . toJSON) data_

app :: IO Application
app = scottyApp $ do
  get "/" $ do
    json (Message "Hello!")
  notFound $ do
    JSON -> json (M.fromList [("error", "nothing to see here, move along.")] :: M.Map String String)

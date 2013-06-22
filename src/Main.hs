module Main (main) where

import           App
import           Network.Wai.Handler.Warp
import           Data.Time (getCurrentTime)

main :: IO ()
main = do
  putStrLn "http://localhost:3000"
  getCurrentTime >>= app >>= run 3000

{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module DiceRoll where

import Control.Monad.IO.Class ()
import Data.ByteString ()
import Data.Serialize ( decode, encode, getWord64host, putWord64host )
import Data.Word ( Word64 )
import Network.Simple.TCP ( connect, recv, send, serve, Socket )
import System.Random ( randomRIO )

diceServer :: IO ()
diceServer = serve "127.0.0.1" "8080" $ \(skt, adr) -> go skt
  where
    go :: Socket -> IO ()
    go skt = do
      -- read and parse request
      mayBytes <- recv skt 8
      case mayBytes of
        Nothing -> pure ()
        Just bytes -> do
          let Right max = decode @Word64 bytes
          response <- encode <$> randomRIO (1, max)
          -- send a response
          send skt response
          go skt

diceClient :: Word64 -> IO Word64
diceClient n = connect "127.0.0.1" "8080" $ \(skt, _) -> do
  send skt (encode n)
  Just bytes <- recv skt 8
  let Right result = decode @Word64 bytes
  pure result

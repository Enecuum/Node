{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
-- import           Crypto.Error
-- import           Crypto.PubKey.ECC.DH
-- import           Crypto.PubKey.ECC.ECDSA
-- import           Crypto.PubKey.ECC.Generate
-- import           Crypto.PubKey.ECC.Types
-- import qualified Data.ByteArray             as BA
-- import qualified Data.ByteString.Lazy       as B
-- import           Data.IORef
-- import           Data.Maybe
-- import           Data.Serialize
-- import           Data.Word
-- import           Network.Socket
-- import           Node.Crypto
-- import           Node.Data.Data

-- import           Boot.Boot
-- import           Boot.Types
-- import           Control.Concurrent
-- import           Control.Concurrent.Async
-- import           Control.Concurrent.Chan
-- import           Control.Monad
-- import           Data.Monoid
-- import           Node.Lib
-- import           Node.Node.Mining
-- import           Node.Node.Types
-- import           Service.Timer

-- import           CLI.CLI

-- import           PoA
-- import           Service.HammingDistance
import           Test.Hspec (describe, hspec, it, shouldReturn)


main :: IO ()
main = hspec $ do

  describe "Basic DB Functionality" $ do
    it "should retrieve n transactions for publickey" $  do
      undefined
      `shouldReturn` (Just "zzz")


parseTXInfoJson :: IO ()
parseTXInfoJson = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = Data.Aeson.encode ti
  print eti
  let res = Data.Aeson.decode eti :: Maybe TransactionInfo
  -- return res
  print $ res


parseTXInfoBin :: IO TransactionInfo
parseTXInfoBin = do
  tx <- genNNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = S.encode ti
  print eti
  let res = decodeThis "TransactionInfo" eti
  return res

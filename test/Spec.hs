{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

import           Data.Aeson
import qualified Data.ByteString.Char8      as BC
import qualified Data.Serialize             as S (encode)
import           Service.Transaction.Common
import           Service.Types
import           Test.Hspec                 (describe, hspec, it, shouldReturn)


main :: IO ()
main = hspec $ do
  describe "Basic DB Functionality" $ do
    it "should retrieve n transactions for publickey" $  do
      retrieveNTransactionsForPublickey
      `shouldReturn` (Nothing)


retrieveNTransactionsForPublickey :: IO (Maybe TransactionInfo)
retrieveNTransactionsForPublickey = return Nothing


parseTXInfoJson :: IO (Maybe TransactionInfo)
parseTXInfoJson = do
  tx <- genNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = Data.Aeson.encode ti
  print eti
  let res = Data.Aeson.decode eti :: Maybe TransactionInfo
  print $ res
  return res


parseTXInfoBin :: IO TransactionInfo
parseTXInfoBin = do
  tx <- genNTx 5
  let ti = TransactionInfo (tx !! 0) (BC.pack "123") 2 False
  let eti = S.encode ti
  print eti
  let res = decodeThis "TransactionInfo" eti
  return res

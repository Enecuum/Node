{-# LANGUAGE OverloadedStrings #-}

import           Enecuum.Prelude
import           Data.Aeson                    as A
                                                ( decode
                                                , encode
                                                )
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                     ( fromJust )
import qualified Data.Serialize                as S
import           Enecuum.Legacy.Service.Transaction.Common
                                                ( decodeThis
                                                , genNTx
                                                )
import           Enecuum.Legacy.Service.Transaction.Generate ( genPoAMicroblock) 
import           Enecuum.Legacy.Service.Transaction.Storage
                                                ( genesisKeyBlock
                                                , getKeyBlockHash
                                                )
import           Enecuum.Legacy.Service.Types   ( TransactionInfo(..) )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                , shouldReturn
                                                )
import           Test.Hspec.Contrib.HUnit       ( fromHUnitTest )
import           Test.HUnit                     ( Test(..)
                                                , (@?=)
                                                )
import           Enecuum.Research.Dsl.HashGraph

main :: IO ()
main = hspec $ do
  describe "Basic DB Functionality" $ 
    it "should retrieve n transactions for publickey"
      $              
                       retrieveNTransactionsForPublickey
      `shouldReturn` Nothing

  describe "Database HUnit tests" $ 
    fromHUnitTest databaseTestSuite

  describe "Parsing HUnit tests" $ 
    fromHUnitTest parsingTestSuite

  describe "Business Logic HUnit tests" $
    fromHUnitTest businessLogicTestSuite

  describe "Network HUnit tests" $ 
    fromHUnitTest networkTestSuite

  describe "Integration HUnit tests" $ 
    fromHUnitTest integrationTestSuite

  describe "CLI and RPC HUnit tests" $ 
    fromHUnitTest cliRPCTestSuite

  describe "HashGraph eDSL tests:" $
    fromHUnitTest hashGraphTestSuit

retrieveNTransactionsForPublickey :: IO (Maybe TransactionInfo)
retrieveNTransactionsForPublickey = return Nothing


parseTXInfoJson :: Test
parseTXInfoJson = TestCase $ do
  tx <- (!! 0) <$> genNTx 5
  let tx1 = TransactionInfo tx (BC.pack "123") 2 False
  let res = fromJust $ A.decode $ A.encode tx1
  res @?= tx1


parseTXInfoBin :: Test
parseTXInfoBin = TestCase $ do
  tx <- (!! 0) <$> genNTx 5
  let tx1 = TransactionInfo tx (BC.pack "123") 2 False
  let res = decodeThis "TransactionInfo" $ S.encode tx1
  res @?= tx1


parseMicroblockJson :: Test
parseMicroblockJson = TestCase $ do
  let hashOfKeyBlock = getKeyBlockHash genesisKeyBlock
  mb <- genPoAMicroblock hashOfKeyBlock
  (fromJust $ A.decode $ A.encode mb) @?= mb


parseMicroblockBin :: Test
parseMicroblockBin = TestCase $ do
  let hashOfKeyBlock = getKeyBlockHash genesisKeyBlock
  mb <- genPoAMicroblock hashOfKeyBlock
  let dmb = decodeThis "Microblock" $ S.encode mb
  dmb @?= mb


checkKeyBlockInfoHash :: Test
checkKeyBlockInfoHash = TestCase $ 
  getKeyBlockHash genesisKeyBlock
    @?= "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="


-- | Parsing HUnit test suite
parsingTestSuite :: Test
parsingTestSuite = TestList
  [ TestLabel "Parse json: TransactionInfo"       parseTXInfoJson
  , TestLabel "Parse json: Microblock"            parseMicroblockJson
  , TestLabel "Parse binary: TransactionInfo"     parseTXInfoBin
  , TestLabel "Parse binary: Microblock"          parseMicroblockBin
  , TestLabel "Transformation: KeyBlockInfo Hash" checkKeyBlockInfoHash
  ]


-- | Business logic HUnit test suite
businessLogicTestSuite :: Test
businessLogicTestSuite = TestList []


-- | Network HUnit test suite
networkTestSuite :: Test
networkTestSuite = TestList []


-- | Database HUnit test suite
databaseTestSuite :: Test
databaseTestSuite = TestList []


-- | Integration HUnit test suite
integrationTestSuite :: Test
integrationTestSuite = TestList []


-- | CLI HUnit test suite
cliRPCTestSuite :: Test
cliRPCTestSuite = TestList []

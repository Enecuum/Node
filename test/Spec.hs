{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

import           Data.Aeson                   as A (decode, encode)
import qualified Data.ByteString.Char8        as BC
import           Data.Either                  (fromRight)
import           Data.Maybe                   (fromJust)
import qualified Data.Serialize               as S (decode, encode)
import           Service.Transaction.Common   (decodeThis, genNTx)
import           Service.Transaction.Generate (genPoAMicroblock,
                                               generateMicroblocksAndKeyBlocks)
import           Service.Transaction.Storage  (genesisKeyBlock, getKeyBlockHash)
import           Service.Types                (Microblock, TransactionInfo (..))
import           Test.Hspec                   (describe, hspec, it,
                                               shouldReturn)
import           Test.Hspec.Contrib.HUnit     (fromHUnitTest)
import           Test.HUnit                   (Test (..), (@?=))


main :: IO ()
main = hspec $ do
  describe "Basic DB Functionality" $ do
    it "should retrieve n transactions for publickey" $  do
      retrieveNTransactionsForPublickey
      `shouldReturn` (Nothing)

  describe "Database HUnit tests" $ do
    fromHUnitTest databaseTestSuite

  describe "Parsing HUnit tests" $ do
    fromHUnitTest parsingTestSuite

  describe "Business Logic HUnit tests" $ do
    fromHUnitTest businessLogicTestSuite

  describe "Network HUnit tests" $ do
    fromHUnitTest networkTestSuite

  describe "Integration HUnit tests" $ do
    fromHUnitTest integrationTestSuite

  describe "CLI and RPC HUnit tests" $ do
    fromHUnitTest cliRPCTestSuite


retrieveNTransactionsForPublickey :: IO (Maybe TransactionInfo)
retrieveNTransactionsForPublickey = return Nothing


parseTXInfoJson :: Test
parseTXInfoJson = TestCase $ do
  tx <- (!! 0) <$> genNTx 5
  let tx1 = TransactionInfo tx (BC.pack "123") 2 False
  let res =  fromJust $ A.decode $ A.encode tx1
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
checkKeyBlockInfoHash = TestCase $ do
  getKeyBlockHash genesisKeyBlock @?= "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="


-- | Parsing HUnit test suite
parsingTestSuite :: Test
parsingTestSuite = TestList [
    TestLabel "Parse json: TransactionInfo" parseTXInfoJson,
    TestLabel "Parse json: Microblock" parseMicroblockJson,
    TestLabel "Parse binary: TransactionInfo" parseTXInfoBin,
    TestLabel "Parse binary: Microblock" parseMicroblockBin,
    TestLabel "Transformation: KeyBlockInfo Hash" checkKeyBlockInfoHash
  ]


-- | Business logic HUnit test suite
businessLogicTestSuite :: Test
businessLogicTestSuite = TestList [
  ]


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

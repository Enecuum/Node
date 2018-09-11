{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Enecuum.Legacy.CryptoSpec where

import           Data.Aeson                    as A
                                                          ( decode
                                                          , encode
                                                          )
import qualified Data.ByteString.Char8         as BC
import           Data.Either                              ( fromRight )
import           Data.Maybe                               ( fromJust )
import qualified Data.Serialize                as S
                                                          ( decode
                                                          , encode
                                                          )
import           Enecuum.Legacy.Service.Transaction.Common
                                                          ( decodeThis
                                                          , genNTx
                                                          )
import           Enecuum.Legacy.Service.Transaction.Generate
                                                          ( genPoAMicroblock
                                                          , generateMicroblocksAndKeyBlocks
                                                          )
import qualified Enecuum.Legacy.Service.Types                      as T
import qualified Enecuum.Legacy.Service.Types.PublicPrivateKeyPair as T
import           Enecuum.Legacy.Service.Types             ( Microblock
                                                          , TransactionInfo (..)
                                                          )

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )
import           Test.HUnit                               ( Test(..)
                                                          , (@?=)
                                                          )

import           Enecuum.Legacy.Refact.Hashing            ( calculateKeyBlockHash )
import           Enecuum.Legacy.Refact.Assets             ( genesisKeyBlock )

import           Prelude

keyBlock :: T.KeyBlockInfoPoW
keyBlock = T.KeyBlockInfoPoW
  { T._time      = 1532005108
  , T._prev_hash = "B1Vh7/LNOtWGd2+pBPAEAoLF9qJh9qj9agpSTRTNLSw="
  , T._number    = 1
  , T._nonce     = 366080
  , T._solver    = "OvS8LmmcMa4mtEWbifO5ZFkqT6AYRizzQ6mEobMMhz4="
  , T._type      = 0
  }

spec :: Spec
spec = do
  describe "Legacy parsing tests" $
    fromHUnitTest parsingTestSuite

  describe "Legacy hahing tests" $ do
    it "Genesis hash calculation" $ do
      let hash = calculateKeyBlockHash genesisKeyBlock
      hash `shouldBe` "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="
    it "Some KeyBlock hash calculation" $ do
      let hash = calculateKeyBlockHash keyBlock
      hash `shouldBe` "AAABrMjWwW95ZXx5EgIn8gG2c0/xaXi1M4uaGWMH28o="

  describe "Legacy serialization tests" $ do
    it "Serialize Transaction" $ do
      let trans = T.Transaction (T.PublicKey256k1 1) (T.PublicKey256k1 1) 1 T.ENQ Nothing Nothing 1
      let encoded = BC.unpack $ S.encode trans
      encoded `shouldBe` ""

-- | Parsing HUnit test suite
parsingTestSuite :: Test
parsingTestSuite = TestList
  [ TestLabel "Parse json: TransactionInfo"       parseTXInfoJson
  , TestLabel "Parse json: Microblock"            parseMicroblockJson
  , TestLabel "Parse binary: TransactionInfo"     parseTXInfoBin
  , TestLabel "Parse binary: Microblock"          parseMicroblockBin
  ]

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
  let hashOfKeyBlock = calculateKeyBlockHash genesisKeyBlock
  mb <- genPoAMicroblock hashOfKeyBlock
  (fromJust $ A.decode $ A.encode mb) @?= mb

parseMicroblockBin :: Test
parseMicroblockBin = TestCase $ do
  let hashOfKeyBlock = calculateKeyBlockHash genesisKeyBlock
  mb <- genPoAMicroblock hashOfKeyBlock
  let dmb = decodeThis "Microblock" $ S.encode mb
  dmb @?= mb

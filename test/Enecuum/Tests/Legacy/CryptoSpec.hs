{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Enecuum.Tests.Legacy.CryptoSpec where

import           Data.Aeson                    as A
                                                          ( decode
                                                          , encode
                                                          )
import qualified Data.ByteString.Char8         as BC
import           Data.Either                              ( fromRight )
import           Data.Maybe                               ( fromJust )
import           Data.List                                ( intercalate )
import qualified Data.Serialize                as S
                                                          ( decode
                                                          , encode
                                                          )
import           Control.Monad                            ( join )
import           Text.Printf                              ( printf )
import           Enecuum.Legacy.Service.System.Directory  ( getTime )
import           Enecuum.Legacy.Service.Transaction.Common
                                                          ( decodeThis
                                                          , genNTx
                                                          )
import           Enecuum.Legacy.Service.Transaction.Generate
                                                          ( genPoAMicroblock
                                                          , generateMicroblocksAndKeyBlocks
                                                          )
import qualified Enecuum.Legacy.Service.Types                      as T
import qualified Enecuum.Legacy.Refact.Crypto.PublicPrivateKeyPair as T
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
import           Enecuum.Legacy.Refact.Crypto.Signing (sign)
import qualified "cryptonite" Crypto.PubKey.ECC.ECDSA            as ECDSA
import           Enecuum.Legacy.Refact.Crypto.Verification (verifyEncodeble)
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

-- owner     = T.PublicKey256k1 1
owner     = read ("PhZLFMzvuukH4fENMRAzqJxyqfVH9Vn856FjEuwbzy9g") :: T.PublicKey
ownerPrivateKey = read ("R9BVmBwnASM2mNBVgmnoxYAMR3bQ8HaGWSrWewm2sqV") :: T.PrivateKey
receiver  = T.PublicKey256k1 1
amount    = (433431 :: T.Amount)
currency  = T.ENQ
timestamp = (Nothing :: Maybe T.Time)
signature = (Nothing :: Maybe T.Signature)
uuid      = (1 :: Int)

transaction :: T.Transaction
transaction = T.Transaction
  { T._owner     = owner
  , T._receiver  = receiver
  , T._amount    = amount
  , T._currency  = currency
  , T._timestamp = timestamp
  , T._signature = signature
  , T._uuid      = uuid
  }

hexForPrint xs = map (printf "%02x") $ BC.unpack $ S.encode $ xs

spec :: Spec
spec = do
  describe "Legacy parsing tests" $
    fromHUnitTest parsingTestSuite

  describe "Legacy hashing tests" $ do
    it "Genesis hash calculation" $ do
      let hash = calculateKeyBlockHash genesisKeyBlock
      hash `shouldBe` "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="
    it "Some KeyBlock hash calculation" $ do
      let hash = calculateKeyBlockHash keyBlock
      hash `shouldBe` "AAABrMjWwW95ZXx5EgIn8gG2c0/xaXi1M4uaGWMH28o="

  describe "Legacy Transaction signing / serialization tests" $ do
    it "Serialize Transaction" $ do
      let txHex = hexForPrint transaction

      let ownerEncoded     :: [String] = hexForPrint owner
      let receiverEncoded  :: [String] = hexForPrint receiver
      let amountEncoded    :: [String] = hexForPrint amount
      let currencyEncoded  :: [String] = hexForPrint currency
      let timestampEncoded :: [String] = hexForPrint timestamp
      let signatureEncoded :: [String] = hexForPrint signature
      let uuidEncoded      :: [String] = hexForPrint uuid

      let txByFieldHex :: [String] = join
            [ ownerEncoded
            , receiverEncoded
            , amountEncoded
            , currencyEncoded
            , timestampEncoded
            , signatureEncoded
            , uuidEncoded
            ]

      txHex `shouldBe` txByFieldHex
      unwords txHex `shouldBe` "01 01 00 00 00 00 00 00 00 21 1f c8 de b7 d7 bf 20 a8 14 8a de 2b 3c c3 ed ba 89 a4 95 13 40 d3 96 6f 8f ff 3b 7a 09 e7 44 51 01 00 00 00 00 01 00 00 00 00 00 06 9d 17 00 00 00 00 00 00 00 00 00 00 01"

    it "Signing / verifying" $ do
      signature <- sign ownerPrivateKey transaction
      let pk = T.getPublicKey $ T.uncompressPublicKey $ owner
      verifyEncodeble pk signature transaction `shouldBe` True

-- | Hex Representation for signed and unsigned transaction
printHexRepresentation :: IO ()
printHexRepresentation = do
  signature <- sign ownerPrivateKey transaction
  let signedTx = transaction {T._signature = Just signature}
  time <- Just <$> getTime
  let signedTxTimeStamp = signedTx {T._timestamp = time}
      hhexPrint xs = unwords $ hexForPrint xs

      transaction'       = hhexPrint transaction
      signature'         = hhexPrint signature
      ownerPrivateKey'   = hhexPrint ownerPrivateKey
      owner'             = hhexPrint owner
      signedTx'          = hhexPrint signedTx
      signedTxTimeStamp' = hhexPrint signedTxTimeStamp
      time'              = hhexPrint time

  mapM_ print ["transaction", show transaction, transaction']
  mapM_ print ["signature", show signature, signature']
  mapM_ print ["ownerPrivateKey", show ownerPrivateKey, ownerPrivateKey']
  mapM_ print ["owner", show owner, owner']
  mapM_ print ["signedTx", show signedTx, signedTx']
  mapM_ print ["signedTxTimeStamp", show signedTxTimeStamp, signedTxTimeStamp']
  mapM_ print ["timestamp", show time, time']

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

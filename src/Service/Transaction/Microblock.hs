{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Microblock where
import Service.Transaction.TransactionsDAG (genNNTx)
import qualified Data.ByteString.Char8 as BC
import qualified "cryptohash" Crypto.Hash.SHA1 as SHA1
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, put, get)
-- import Service.Types (Time, Microblock(..),Transaction(..))
import Service.Types (MicroblockV1(..), Microblock(..), Transaction)
import Service.Types.PublicPrivateKeyPair (KeyPair(..), PublicKey(..), generateNewRandomAnonymousKeyPair, getSignature) -- Signature)
-- import Data.Aeson as A
import Service.Transaction.Common (runLedger, connectAndRecoverRocks, addMicroblockToDB)

type HashOfMicroblock = BC.ByteString

genNMicroBlocksV1 :: Int -> IO [MicroblockV1]
genNMicroBlocksV1 n = evalStateT (replicateM n genMicroBlockV1) BC.empty


genMicroBlockV1 :: StateT HashOfMicroblock IO MicroblockV1
genMicroBlockV1 = do
  aHashPreviousMicroblock <- get
  n <- lift $ randomRIO (3,4) --(40,128) -- from 40 to 128 transactions per block
  tx <- lift $ genNNTx n
  let aHashCurrentMicroblock = (SHA1.hash . BC.pack . show) tx
  put aHashCurrentMicroblock
  return (MicroblockV1 aHashCurrentMicroblock aHashPreviousMicroblock tx)


testParseTx :: IO ()
testParseTx = do
  let h01 = read "B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN" :: PublicKey
  let h02 = "B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN"
  putStrLn $ show $ (read (h02) :: PublicKey)
  putStrLn $ show $ (read (show h01) :: PublicKey)
  let tx1 = "WithTime {time = 48813.639349002, transaction = RegisterPublicKey {pubKey = B0WvuBJEsQQ6RVqaEFaWoGXmW8sMG8xnBuxaCXJycRjyXN, startWithBalance = 25}}"
  putStrLn $ show $ (read tx1 :: Transaction)


writeTxToFile quantityOfTx = do
  tx <- genNNTx quantityOfTx
  writeFile "txforcli.txt" $ unlines $ map show tx

-- runLedgerForTx = do
--   content <- lines <$> readFile "txforcli.txt"
--   let tx = map (\t -> read t :: Transaction) content
--   (KeyPair signerPublicKey signerPrivateKey) <- generateNewRandomAnonymousKeyPair
--   keys <- replicateM 64 generateNewRandomAnonymousKeyPair
--   let teamKeys = map (\(KeyPair pub _) -> pub) keys
--   sign  <- getSignature signerPrivateKey (unlines (map show tx))
--   let mb = Microblock {_keyBlock = BC.pack "321",
--                        _signer = signerPublicKey,
--                        _sign = sign,
--                        _teamKeys = teamKeys,
--                        _transactions = tx,
--                        _numOfBlock = 123}
--   db <- connectRocks
--   addMicroblockToDB db mb
--   runLedger db mb
--   -- putStrLn ht

-- writeMicroblockToFile = undefined
-- runLedgerForMB = undefined


-- calculateLedgerForTransaction = writeTxToFile 20 >> runLedgerForTx
-- calculateLedgerForMicroblock = writeMicroblockToFile 4 1 >> runLedgerForMB


genNMicroBlocks n = replicateM n genMicroBlock
genMicroBlock quantityOfTx = do
  tx <- genNNTx quantityOfTx
  (KeyPair signerPublicKey signerPrivateKey) <- generateNewRandomAnonymousKeyPair
  keys <- replicateM 64 generateNewRandomAnonymousKeyPair
  let teamKeys = map (\(KeyPair pub _) -> pub) keys
  sign  <- getSignature signerPrivateKey (unlines (map show tx))
  let mb = Microblock {_keyBlock = BC.pack "321",
                       _signer = signerPublicKey,
                       _sign = sign,
                       _teamKeys = teamKeys,
                       _transactions = tx,
                       _numOfBlock = 123}
  return mb

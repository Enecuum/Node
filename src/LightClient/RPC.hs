{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LightClient.RPC (
        newTx,
        genNTx, 
        genUnlimTx,
        reqLedger,
     ) where

import Network.JsonRpc.Client
import qualified Data.ByteString.Lazy as B
import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, void, forM, forM_)
import Control.Monad.Except (runExceptT, liftIO)
import Network.Socket.ByteString (sendAllTo, recv)

import Control.Timeout (timeout)
import Data.Time.Units (Second)
import Service.Types (Transaction)
import Service.Types.PublicPrivateKeyPair hiding (Signature)
import Service.Network.UDP.Client
import Service.Types.SerializeJSON ()

type Result a = RpcResult IO a


-- Client-side function's signature
newTxSig :: Signature (Transaction ::: ()) ()
newTxSig = Signature "new_tx" ("x" ::: ())

genNTxSig :: Signature (Int ::: ()) ()
genNTxSig = Signature "gen_n_tx" ("x" ::: ())

genUnlimTxSig :: Signature () ()
genUnlimTxSig = Signature "gen_unlim_tx" ()

reqLedgerSig :: Signature (PublicKey ::: ()) Amount
reqLedgerSig = Signature "get_balance" ("x" ::: ())

-- Bind function signature with RPC connection
newTx :: ClientHandle -> Transaction -> Result ()
newTx h tx = toFunction (connectionWithTimeOut h) newTxSig tx

genNTx :: ClientHandle -> Int -> Result ()
genNTx h num = toFunction (connectionWithTimeOut h) genNTxSig num

genUnlimTx :: ClientHandle -> Result ()
genUnlimTx h = toFunction (connectionWithTimeOut h) genUnlimTxSig

reqLedger :: ClientHandle -> PublicKey -> Result Amount
reqLedger h key = toFunction (connectionWithTimeOut h) reqLedgerSig key




connectionWithTimeOut :: ClientHandle -> Connection IO
connectionWithTimeOut h input = do
  result <- timeout (5 :: Second) $ connection h input
  case result of
    Just a -> return a
    Nothing -> return (error "Connection error: out of time-out")

-- Connect to a server
connection :: ClientHandle -> Connection IO
connection handle input = do
    void $ sendAllTo
           (clientSocket handle) (B.toStrict input) (clientAddress handle)
    ans <- recv (clientSocket handle) (1024*10)
    return (Just (B.fromStrict ans))


{-
genTxDAG :: IO [Transaction]
genTxDAG = do
    keys <- replicateM 10 K.generateNewRandomAnonymousKeyPair
    dag <- getTransactionDAG keys
    forM (labEdges dag) $ \(_, _, tr) -> return tr

-- Usual sending signle tx
runRpcs :: Result ()
runRpcs = do
   result <- reqLedger (publicKey256k1 0)
   liftIO $ putStrLn $ show result

main = do
  result <- runExceptT runRpcs
  return ()
-}

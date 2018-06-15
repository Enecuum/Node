{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LightClient.RPC (
        newTx,
        reqLedger,
        getBlock,
        getTx,
        getAllTxs,

--test
        genNTx,
        genUnlimTx,
        newMsgBroadcast,
        newMsgTo,
        loadNewMsg,

        QuantityTx,
        PubKey,
        Trans(..)
     ) where

import Network.JsonRpc.Client
import qualified Data.ByteString.Lazy as B
import Control.Monad (void)
import Network.Socket.ByteString (sendAllTo, recv)

import Control.Timeout (timeout)
import Data.Time.Units (Second)
import Service.Types
import Service.Types.PublicPrivateKeyPair hiding (Signature)
import Service.Network.Base
import Service.Types.SerializeJSON ()


type Result a = RpcResult IO a


-- Client-side function's signature
newTxSig :: Signature (Transaction ::: ()) ()
newTxSig = Signature "enq_sendTransaction" ("tx" ::: ())

reqLedgerSig :: Signature (PubKey ::: ()) Amount
reqLedgerSig = Signature "enq_getBalance" ("address" ::: ())

reqGetBlockSig :: Signature (Hash ::: ()) MicroblockV1
reqGetBlockSig = Signature "enq_getBlockByHash" ("hash" ::: ())

reqGetTxSig :: Signature (Hash ::: ()) TransactionInfo
reqGetTxSig = Signature "enq_getTransactionByHash" ("hash" ::: ())

reqGetAllTxsSig :: Signature (PubKey ::: ()) [Transaction]
reqGetAllTxsSig = Signature "enq_getAllTransactions" ("address" ::: ())

--test
genNTxSig :: Signature (QuantityTx ::: ()) ()
genNTxSig = Signature "gen_n_tx" ("x" ::: ())

genUnlimTxSig :: Signature () ()
genUnlimTxSig = Signature "gen_unlim_tx" ()

newMsgBroadcastSig :: Signature (String ::: ()) ()
newMsgBroadcastSig = Signature "send_message_broadcast" ("x" ::: ())

newMsgToSig :: Signature (MsgTo ::: ()) ()
newMsgToSig = Signature "send_message_to" ("x" ::: ())

loadNewMsgSig :: Signature () [MsgTo]
loadNewMsgSig = Signature "load_messages" ()

-- Bind function signature with RPC connection
newTx :: ClientHandle -> Transaction -> Result ()
newTx h = toFunction (connectionWithTimeOut h) newTxSig

reqLedger :: ClientHandle -> PubKey -> Result Amount
reqLedger h = toFunction (connectionWithTimeOut h) reqLedgerSig

getBlock :: ClientHandle -> Hash -> Result MicroblockV1
getBlock h = toFunction (connectionWithTimeOut h) reqGetBlockSig

getTx :: ClientHandle -> Hash -> Result TransactionInfo
getTx h = toFunction (connectionWithTimeOut h) reqGetTxSig

getAllTxs :: ClientHandle -> PubKey -> Result [Transaction]
getAllTxs h = toFunction (connectionWithTimeOut h) reqGetAllTxsSig


--test
genNTx :: ClientHandle -> Int -> Result ()
genNTx h = toFunction (connectionWithTimeOut h) genNTxSig

genUnlimTx :: ClientHandle -> Result ()
genUnlimTx h = toFunction (connectionWithTimeOut h) genUnlimTxSig

newMsgBroadcast :: ClientHandle -> String -> Result ()
newMsgBroadcast h = toFunction (connectionWithTimeOut h) newMsgBroadcastSig

newMsgTo :: ClientHandle -> MsgTo -> Result ()
newMsgTo h = toFunction (connectionWithTimeOut h) newMsgToSig

loadNewMsg :: ClientHandle -> Result [MsgTo]
loadNewMsg h = toFunction (connectionWithTimeOut h) loadNewMsgSig


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

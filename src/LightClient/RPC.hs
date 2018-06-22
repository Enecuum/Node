{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LightClient.RPC (
        newTx,
        reqLedger,
        getBlock,
        getMicroblock,
        getTx,
        getAllTxs,
        getChainInfo,

--test
        genNTx,
        genUnlimTx,
        newMsgBroadcast,
        newMsgTo,
        loadNewMsg,

        QuantityTx,
        Trans(..)
     ) where

import Network.JsonRpc.Client
import Control.Monad (void)
import qualified Network.WebSockets as WS

import Control.Timeout (timeout)
import Data.Time.Units (Second)
import Service.Types
import Service.Types.PublicPrivateKeyPair hiding (Signature)
import Service.Types.SerializeJSON ()


type Result a = RpcResult IO a


-- Client-side function's signature
newTxSig :: Signature (Transaction ::: ()) ()
newTxSig = Signature "enq_sendTransaction" ("tx" ::: ())

reqLedgerSig :: Signature (PublicKey ::: ()) Amount
reqLedgerSig = Signature "enq_getBalance" ("address" ::: ())

reqGetBlockSig :: Signature (Hash ::: ()) Macroblock
reqGetBlockSig = Signature "enq_getBlockByHash" ("hash" ::: ())

reqGetMicroblockSig :: Signature (Hash ::: ()) MicroblockAPI
reqGetMicroblockSig = Signature "enq_getMicroblockByHash" ("hash" ::: ())

reqGetTxSig :: Signature (Hash ::: ()) TransactionInfo
reqGetTxSig = Signature "enq_getTransactionByHash" ("hash" ::: ())

reqGetAllTxsSig :: Signature (PublicKey ::: ()) [Transaction]
reqGetAllTxsSig = Signature "enq_getAllTransactions" ("address" ::: ())

reqChainInfoSig :: Signature () ChainInfo
reqChainInfoSig = Signature "enq_getChainInfo" ()

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
newTx :: WS.Connection -> Transaction -> Result ()
newTx h = toFunction (connectionWithTimeOut h) newTxSig

reqLedger :: WS.Connection -> PublicKey -> Result Amount
reqLedger h = toFunction (connectionWithTimeOut h) reqLedgerSig

getBlock :: WS.Connection -> Hash -> Result Macroblock
getBlock h = toFunction (connectionWithTimeOut h) reqGetBlockSig

getMicroblock :: WS.Connection -> Hash -> Result MicroblockAPI
getMicroblock h = toFunction (connectionWithTimeOut h) reqGetMicroblockSig

getTx :: WS.Connection -> Hash -> Result TransactionInfo
getTx h = toFunction (connectionWithTimeOut h) reqGetTxSig

getAllTxs :: WS.Connection -> PublicKey -> Result [Transaction]
getAllTxs h = toFunction (connectionWithTimeOut h) reqGetAllTxsSig

getChainInfo :: WS.Connection -> Result ChainInfo
getChainInfo h = toFunction (connectionWithTimeOut h) reqChainInfoSig


--test
genNTx :: WS.Connection -> Int -> Result ()
genNTx h = toFunction (connectionWithTimeOut h) genNTxSig

genUnlimTx :: WS.Connection -> Result ()
genUnlimTx h = toFunction (connectionWithTimeOut h) genUnlimTxSig

newMsgBroadcast :: WS.Connection -> String -> Result ()
newMsgBroadcast h = toFunction (connectionWithTimeOut h) newMsgBroadcastSig

newMsgTo :: WS.Connection -> MsgTo -> Result ()
newMsgTo h = toFunction (connectionWithTimeOut h) newMsgToSig

loadNewMsg :: WS.Connection -> Result [MsgTo]
loadNewMsg h = toFunction (connectionWithTimeOut h) loadNewMsgSig

connectionWithTimeOut :: WS.Connection -> Connection IO
connectionWithTimeOut h input = do
  result <- timeout (5 :: Second) $ connection h input
  case result of
    Just a -> return a
    Nothing -> return (error "Connection error: out of time-out")

-- Connect to a server
connection :: WS.Connection -> Connection IO
connection handle input = do
    void $ WS.sendTextData handle input
    ans <- WS.receiveData handle
    return (Just ans)


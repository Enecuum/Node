{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module LightClient.RPC (
        newTx,
        reqLedger,
        getBlock,
        getMicroblock,
        getTx,
        getAllTxs,
        getPartTxs,
        getChainInfo,

--test
        newMsgBroadcast,
        newMsgTo,
        loadNewMsg,

        QuantityTx,
        Trans(..)
     ) where

import           Control.Monad                      (void)
import           Network.JsonRpc.Client
import qualified Network.WebSockets                 as WS

import           Control.Timeout                    (timeout)
import           Data.Time.Units                    (Second)
import           Service.Types
import           Service.Types.PublicPrivateKeyPair hiding (Signature)
import           Service.Types.SerializeJSON        ()


type Result a = RpcResult IO a


-- Client-side function's signature
newTxSig :: Signature (Transaction ::: ()) Hash
newTxSig = Signature "enq_sendTransaction" ("tx" ::: ())

reqLedgerSig :: Signature (PublicKey ::: ()) Amount
reqLedgerSig = Signature "enq_getBalance" ("address" ::: ())

reqGetBlockSig :: Signature (Hash ::: ()) MacroblockAPI
reqGetBlockSig = Signature "enq_getBlockByHash" ("hash" ::: ())

reqGetMicroblockSig :: Signature (Hash ::: ()) MicroblockAPI
reqGetMicroblockSig = Signature "enq_getMicroblockByHash" ("hash" ::: ())

reqGetTxSig :: Signature (Hash ::: ()) TransactionInfo
reqGetTxSig = Signature "enq_getTransactionByHash" ("hash" ::: ())

reqGetAllTxsSig :: Signature (PublicKey ::: ()) [TransactionAPI]
reqGetAllTxsSig = Signature "enq_getAllTransactions" ("address" ::: ())

reqGetPartTxsSig :: Signature (PublicKey ::: Integer ::: Integer ::: ()) [TransactionAPI]
reqGetPartTxsSig = Signature "enq_getTransactionsByWallet" ("address" ::: "offset" ::: "count" ::: ())

reqChainInfoSig :: Signature () ChainInfo
reqChainInfoSig = Signature "enq_getChainInfo" ()

--test
newMsgBroadcastSig :: Signature (String ::: ()) ()
newMsgBroadcastSig = Signature "send_message_broadcast" ("x" ::: ())

newMsgToSig :: Signature (MsgTo ::: ()) ()
newMsgToSig = Signature "send_message_to" ("x" ::: ())

loadNewMsgSig :: Signature () [MsgTo]
loadNewMsgSig = Signature "load_messages" ()

-- Bind function signature with RPC connection
newTx :: WS.Connection -> Transaction -> Result Hash
newTx h = toFunction (connectionWithTimeOut h) newTxSig

reqLedger :: WS.Connection -> PublicKey -> Result Amount
reqLedger h = toFunction (connectionWithTimeOut h) reqLedgerSig

getBlock :: WS.Connection -> Hash -> Result MacroblockAPI
getBlock h = toFunction (connectionWithTimeOut h) reqGetBlockSig

getMicroblock :: WS.Connection -> Hash -> Result MicroblockAPI
getMicroblock h = toFunction (connectionWithTimeOut h) reqGetMicroblockSig

getTx :: WS.Connection -> Hash -> Result TransactionInfo
getTx h = toFunction (connectionWithTimeOut h) reqGetTxSig

getAllTxs :: WS.Connection -> PublicKey -> Result [TransactionAPI]
getAllTxs h = toFunction (connectionWithTimeOut h) reqGetAllTxsSig

getPartTxs :: WS.Connection -> PublicKey -> Integer -> Integer -> Result [TransactionAPI]
getPartTxs h = toFunction (connectionWithTimeOut h) reqGetPartTxsSig

getChainInfo :: WS.Connection -> Result ChainInfo
getChainInfo h = toFunction (connectionWithTimeOut h) reqChainInfoSig


--test
newMsgBroadcast :: WS.Connection -> String -> Result ()
newMsgBroadcast h = toFunction (connectionWithTimeOut h) newMsgBroadcastSig

newMsgTo :: WS.Connection -> MsgTo -> Result ()
newMsgTo h = toFunction (connectionWithTimeOut h) newMsgToSig

loadNewMsg :: WS.Connection -> Result [MsgTo]
loadNewMsg h = toFunction (connectionWithTimeOut h) loadNewMsgSig

connectionWithTimeOut :: WS.Connection -> Connection IO
connectionWithTimeOut h input = do
  result <- timeout (10 :: Second) $ connection h input
  case result of
    Just a  -> return a
    Nothing -> return (error "Connection error: out of time-out")

-- Connect to a server
connection :: WS.Connection -> Connection IO
connection handle input = do
    void $ WS.sendTextData handle input
    ans <- WS.receiveData handle
    return (Just ans)

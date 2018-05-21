{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LightClient.RPC (
        newTx,
        genNTx,
        genUnlimTx,
        genNewKey,
        getKeys,
        reqLedger,

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
newTxSig :: Signature (Trans ::: ()) Transaction
newTxSig = Signature "new_tx" ("x" ::: ())

genNTxSig :: Signature (QuantityTx ::: ()) ()
genNTxSig = Signature "gen_n_tx" ("x" ::: ())

genUnlimTxSig :: Signature () ()
genUnlimTxSig = Signature "gen_unlim_tx" ()

genNewKeySig :: Signature () PubKey
genNewKeySig = Signature "gen_new_key" ()

getKeysSig :: Signature () [PubKey]
getKeysSig = Signature "get_keys" ()

reqLedgerSig :: Signature (PubKey ::: ()) Amount
reqLedgerSig = Signature "get_balance" ("x" ::: ())

-- Bind function signature with RPC connection
newTx :: ClientHandle -> Trans -> Result Transaction
newTx h = toFunction (connectionWithTimeOut h) newTxSig

genNTx :: ClientHandle -> Int -> Result ()
genNTx h = toFunction (connectionWithTimeOut h) genNTxSig

genUnlimTx :: ClientHandle -> Result ()
genUnlimTx h = toFunction (connectionWithTimeOut h) genUnlimTxSig

genNewKey :: ClientHandle -> Result PubKey
genNewKey h = toFunction (connectionWithTimeOut h) genNewKeySig

getKeys :: ClientHandle -> Result [PubKey]
getKeys h = toFunction (connectionWithTimeOut h) getKeysSig

reqLedger :: ClientHandle -> PubKey -> Result Amount
reqLedger h = toFunction (connectionWithTimeOut h) reqLedgerSig




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

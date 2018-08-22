{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module LightClient.RPC (
        newTx,
        reqLedger,

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
newTxSig = Signature "sendTransaction" ("tx" ::: ())

reqLedgerSig :: Signature (PublicKey ::: ()) Amount
reqLedgerSig = Signature "getBalance" ("hash" ::: ())


-- Bind function signature with RPC connection
newTx :: WS.Connection -> Transaction -> Result Hash
newTx h = toFunction (connectionWithTimeOut h) newTxSig

reqLedger :: WS.Connection -> PublicKey -> Result Amount
reqLedger h = toFunction (connectionWithTimeOut h) reqLedgerSig


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

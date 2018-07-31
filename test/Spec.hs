{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
import           Crypto.Error
import           Crypto.PubKey.ECC.DH
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types
import qualified Data.ByteArray             as BA
import qualified Data.ByteString.Lazy       as B
import           Data.IORef
import           Data.Maybe
import           Data.Serialize
import           Data.Word
import           Network.Socket
import           Node.Crypto
import           Node.Data.Data

import           Boot.Boot
import           Boot.Types
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad
import           Data.Monoid
import           Node.Lib
import           Node.Node.Mining
import           Node.Node.Types
import           Service.Timer

import           CLI.CLI

import           PoA
import           Service.HammingDistance


main :: IO ()
main = undefined

-- check,
{-
ResponseNetLvlPackage
    (RequestNetLvlPackage BroadcastListRequest
        (PackageSignature
            (MyNodeId 1)
            (TimeSpec {sec = 1524216260, nsec = 508425014})
            (Signature {sign_r = 1, sign_s = 1})))
    (BroadcastListResponse
        (NodeInfoListLogicLvl [])
        (NodeInfoListNetLvl [(NodeId 1,1,1)]))
        (PackageSignature
            (MyNodeId 1)
            (TimeSpec {sec = 1, nsec = 1})
            (Signature {sign_r = 1, sign_s = 1}))
-}

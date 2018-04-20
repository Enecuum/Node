{-# Language OverloadedStrings, PackageImports #-}
import                  Node.Data.Data
import                  Node.Crypto
import                  Data.Serialize
import                  Data.Maybe
import                  Data.Word
import                  Network.Socket
import qualified        Data.ByteString.Lazy      as B
import qualified        Data.ByteArray            as BA
import                  Data.IORef
import                  Crypto.PubKey.ECC.DH
import                  Crypto.PubKey.ECC.ECDSA
import                  Crypto.PubKey.ECC.Types
import                  Crypto.Error
import                  Crypto.PubKey.ECC.Generate

import                  Node.Lib
import                  Boot.Boot
import                  Boot.Types
import                  Node.Node.Mining
import                  Node.Node.Types
import                  Service.Timer
import                  Control.Monad
import                  Data.Monoid
import                  Control.Concurrent
import                  Control.Concurrent.Chan
import                  Control.Concurrent.Async

import                  CLI.CLI

import                  Service.HammingDistance
import                  PoA


main :: IO ()
main = undefined


{-
ResponceNetLvlPackage
    (RequestNetLvlPackage BroadcastListRequest
        (PackageSignature
            (MyNodeId 1)
            (TimeSpec {sec = 1524216260, nsec = 508425014})
            (Signature {sign_r = 1, sign_s = 1})))
    (BroadcastListResponce
        (NodeInfoListLogicLvl [])
        (NodeInfoListNetLvl [(NodeId 1,1,1)]))
        (PackageSignature
            (MyNodeId 1)
            (TimeSpec {sec = 1, nsec = 1})
            (Signature {sign_r = 1, sign_s = 1}))
-}

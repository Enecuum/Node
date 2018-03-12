{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CLI.TransactionsDAG where --(transactionProcc, getTransactionDAG) where

import Data.Graph.Inductive
import Control.Monad (replicateM)
import Service.Types.PublicPrivateKeyPair
-- import Skelet
-- import Data.Time.Clock (getCurrentTime, utctDayTime)
-- import System.CPUTime (getCPUTime)
import System.Random
-- import GHC.Generics
import Service.System.Directory (getTime)
import Service.Types
import CLI.Skelet (getSkeletDAG)



getLabsNodes :: [LNode a] -> [a]
getLabsNodes = map snd

getLabsEdges :: Gr a b -> [b]
getLabsEdges = map (\(_,_,l) -> l) . labEdges


addLabels :: [((a, b1), (b2, b3))] -> [c] -> [(a, b2, c)]
addLabels = zipWith (\((n1,_), (n2,_)) l -> (n1, n2, l))

getTransactions :: [PublicKey] -> (Int, Int) -> IO [Transaction]
getTransactions keys (x,y) = do
  let n = length keys
  sums   <- replicateM n $ randomRIO (x,y)
  points <- replicateM n getTime
  let res = [WithTime p (RegisterPublicKey k (fromIntegral s)) | p <- points, k <- keys, s <- sums]
  return res

getSignTransactions :: [LNode KeyPair] -> (Int, Int) -> IO [LEdge Transaction]
getSignTransactions keys'ns (x,y) = do
  let skel = getSkeletDAG keys'ns
  let n    = length skel
  -- let n    = length keys'ns
  let keys = getLabsNodes keys'ns
  sums   <- replicateM n $ randomRIO (x,y)
  points <- replicateM n getTime
  signs  <- mapM (\(KeyPair _ priv, s) -> getSignature priv (fromIntegral s :: Amount)) (zip keys sums)
  let sts  = [WithSignature (WithTime p (SendAmountFromKeyToKey pub1 pub2 (fromIntegral aSum))) sign |
              p <- points, ( (_, (KeyPair pub1 _)), (_, (KeyPair pub2 _)) ) <- skel, aSum <- sums, sign <- signs]
  return $ addLabels skel sts

getTransactionDAG :: [KeyPair] -> IO DAG
getTransactionDAG keys = do
  let n       = length keys
  let pubs    = map (\(KeyPair pub _) -> pub) keys
  ts     <- getTransactions pubs (20,30) --keys (20,30)
  let ns      = zip [1..n] ts
  let keys'ns = zip [1..n] keys
  es     <- getSignTransactions keys'ns (10,20)
  return $ mkGraph ns es

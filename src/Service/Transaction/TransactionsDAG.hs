{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}

module Service.Transaction.TransactionsDAG where

import Data.Graph.Inductive
import Control.Monad (replicateM)
import Service.Types.PublicPrivateKeyPair
import System.Random
import Service.System.Directory (getTime)
import Service.Types
import Service.Transaction.Skelet (getSkeletDAG)

type QuantityOfTransactions = Int

getLabsNodes :: [LNode a] -> [a]
getLabsNodes = map snd

getLabsEdges :: Gr a b -> [b]
getLabsEdges = map (\(_,_,l) -> l) . labEdges


addLabels :: [((a, b1), (b2, b3))] -> [c] -> [(a, b2, c)]
addLabels = zipWith (\((n1,_), (n2,_)) l -> (n1, n2, l))

getRegisterPublicKeyTransactions :: [PublicKey] -> (Int, Int) -> IO [Transaction]
getRegisterPublicKeyTransactions keys (x,y) = do
  let n = length keys
  sums   <- replicateM n $ randomRIO (x,y)
  points <- replicateM n getTime
  let res = take n [WithTime p (RegisterPublicKey k (fromIntegral s)) | p <- points, k <- keys, s <- sums]
  return res

getSignTransactions :: Int -> [LNode KeyPair] -> (Int, Int) -> IO [Transaction] --[LEdge Transaction]
getSignTransactions quantityOfTx keys'ns (x,y) = do
  let skel = getSkeletDAG keys'ns
  let n    = length skel
  let keys = getLabsNodes keys'ns
  sums   <- replicateM n $ randomRIO (x,y)
  points <- replicateM n getTime
  signs  <- mapM (\(KeyPair _ priv, s) -> getSignature priv (fromIntegral s :: Amount)) (zip keys sums)
  let sts  = [WithSignature (WithTime p (SendAmountFromKeyToKey pub1 pub2 (fromIntegral aSum))) sign |
              p <- points, ((_, KeyPair pub1 _), (_, KeyPair pub2 _) ) <- skel, aSum <- sums, sign <- signs]
  return (take quantityOfTx sts)

getTransactions :: [KeyPair] -> QuantityOfTransactions-> IO [Transaction] --IO DAG
getTransactions keys quantityTx = do
  let quantityRegistereKeyTx       = length keys
  let pubs    = map (\(KeyPair pub _) -> pub) keys
  registereKeyTx     <- getRegisterPublicKeyTransactions pubs (20,30)
  let keys'ns = zip [1..quantityRegistereKeyTx] keys
  let quantityBasicTx = quantityTx-quantityRegistereKeyTx
  basicTx <- loopTransaction keys'ns quantityBasicTx
  return ( registereKeyTx ++ basicTx)

-- generate N transactions
genNNTx :: Int -> IO [Transaction]
genNNTx quantityOfTx = do
    let ratioKeysToTx = 3
        qKeys = div quantityOfTx ratioKeysToTx
        quantityOfKeys = if qKeys < 2 then 2 else qKeys

    keys <- replicateM quantityOfKeys generateNewRandomAnonymousKeyPair
    getTransactions keys quantityOfTx


-- accumulate Transcations in acc until it satisfies required Quantity Of Transactions
loopTransaction :: [LNode KeyPair] -> Int -> IO [Transaction]
loopTransaction keys'ns requiredQuantityOfTransactions = loop []
  where
   loop acc = do
     let amountRange = (10,20)
     tx <- getSignTransactions requiredQuantityOfTransactions keys'ns amountRange
     if length acc >= requiredQuantityOfTransactions then return acc else loop (tx ++ acc)

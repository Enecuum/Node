{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}

module Service.Transaction.TransactionsDAG where

import Data.Graph.Inductive
import Control.Monad (replicateM)
import Service.Types.PublicPrivateKeyPair
import Service.System.Directory (getTime)
import Service.Types
import Service.Transaction.Skelet (getSkeletDAG)
import System.Random
type QuantityOfTransactions = Int

getLabsNodes :: [LNode a] -> [a]
getLabsNodes = map snd

getLabsEdges :: Gr a b -> [b]
getLabsEdges = map (\(_,_,l) -> l) . labEdges


addLabels :: [((a, b1), (b2, b3))] -> [c] -> [(a, b2, c)]
addLabels = zipWith (\((n1,_), (n2,_)) l -> (n1, n2, l))

getSignTransactions :: Int -> [LNode KeyPair] -> (Int, Int) -> IO [Transaction] --[LEdge Transaction]
getSignTransactions quantityOfTx keys'ns (x,y) = do
  let skel = getSkeletDAG keys'ns
  let n    = length skel
  let keys = getLabsNodes keys'ns
  sums   <- replicateM n $ randomRIO (x,y)
  points <- replicateM n getTime
  signs  <- mapM (\(KeyPair _ priv, s) -> getSignature priv (fromIntegral s :: Amount)) (zip keys sums)
  uuids <- replicateM n $ randomRIO (1,25)
  let sts  = [Transaction pub1 pub2 (fromIntegral aSum) ENQ (Just p) (Just sign) uuid |
              p <- points, ((_, KeyPair pub1 _), (_, KeyPair pub2 _) ) <- skel, aSum <- sums, sign <- signs, uuid <- uuids ]
  return (take quantityOfTx sts)

getTransactions :: [KeyPair] -> QuantityOfTransactions-> IO [Transaction] --IO DAG
getTransactions keys quantityTx = do
  let quantityRegistereKeyTx       = length keys
  let pubs    = map (\(KeyPair pub _) -> pub) keys
  let keys'ns = zip [1..quantityRegistereKeyTx] keys
  let quantityBasicTx = quantityTx-quantityRegistereKeyTx
  basicTx <- loopTransaction keys'ns quantityBasicTx
  return basicTx


-- accumulate Transcations in acc until it satisfies required Quantity Of Transactions
loopTransaction :: [LNode KeyPair] -> Int -> IO [Transaction]
loopTransaction keys'ns requiredQuantityOfTransactions = loop []
  where
   loop acc = do
     let amountRange = (10,20)
     tx <- getSignTransactions requiredQuantityOfTransactions keys'ns amountRange
     if length acc >= requiredQuantityOfTransactions then return acc else loop (tx ++ acc)


-- generate N transactions
genNNTx :: Int -> IO [Transaction]
genNNTx quantityOfTx = do
    let ratioKeysToTx = 3
        qKeys = div quantityOfTx ratioKeysToTx
        quantityOfKeys = if qKeys < 2 then 2 else qKeys

    keys <- replicateM quantityOfKeys generateNewRandomAnonymousKeyPair
    getTransactions keys quantityOfTx


-- generate N transactions
genNTx :: Int -> IO [Transaction]
genNTx n = do
   let quantityOfKeys = if qKeys <= 2 then 2 else qKeys
                        where qKeys = div n 3
   keys <- replicateM quantityOfKeys generateNewRandomAnonymousKeyPair
   tx <- getTransactions keys n
   return tx

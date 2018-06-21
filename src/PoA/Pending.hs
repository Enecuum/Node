{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf#-}
module PoA.Pending where

import Data.Sequence as S
import Control.Concurrent.Chan
import Control.Monad
import Control.Concurrent
import Service.Types
import System.Clock
import Data.Foldable

import Control.Monad.Fix
import Service.Types.PublicPrivateKeyPair
import Service.InfoMsg
import Node.Data.GlobalLoging
-- actor
-- actor's data
-- commands for actor
--
-- add transaction
-- get n transaction (or less than n)
-- delete transaction from microblock
-- get rid of transactions (out of memory)

-- run transactions cleaning if there are a lot
-- memory check (now - 1k txs in pending)
-- 2 queues ni a pending structure

-- dupChan structure (microblock chan)
-- get from transaction chan and pack into the message of another type

-- Block -> RemoveTransactions [Transactions]
-- Transaction -> AddTransaction Transaction


data PendingAction where
    RemoveTransactions  :: [Transaction]           -> PendingAction
    AddTransaction      :: Transaction              -> PendingAction
    GetTransaction      :: Int -> Chan [Transaction]-> PendingAction


data Pending = Pending (Seq (Transaction, TimeSpec)) (Seq (Transaction, TimeSpec))


pendingActor :: Chan PendingAction -> Chan Microblock -> Chan Transaction -> Chan InfoMsg -> IO ()
pendingActor aChan aMicroblockChan aTransactionChan aInfoChan = do
{-
    writeLog aInfoChan [PendingTag, InitTag] Info "Init. Pending actor for microblocs"
    void . forkIO $ do
        aBlockChan <- dupChan aMicroblockChan
        -- blocks re-pack
        forever $ readChan aBlockChan >>= \case
            Microblock _ _ _ aTransactions _ -> do
                writeLog aInfoChan [PendingTag, InitTag] Info "Repacking of transactions"
                writeChan aChan $ RemoveTransactions aTransactions
-}
    -- transactions re-pack
    writeLog aInfoChan [PendingTag, InitTag] Info "Init. Pending actor for transactions"
    void . forkIO $ forever $ forever $ readChan aTransactionChan >>=
        writeChan aChan . AddTransaction

    -- actor's main body
    writeLog aInfoChan [PendingTag, InitTag] Info "Init. Pending actor for commands"

    void $ loop $ Pending Empty Empty
  where
    loop (Pending aNewTransaactions aOldTransactions) = readChan aChan >>= \case
        AddTransaction aTransaction -> do
            writeLog aInfoChan [PendingTag] Info $
                "Add transaction to pending" ++ show aTransaction
            aNaw <- getTime Realtime
            let aSizeOfOldTransactions = S.length aOldTransactions
                aSizeOfNewTransactions = S.length aNewTransaactions
                aSize = aSizeOfNewTransactions + aSizeOfOldTransactions
                aAverage = (average aNewTransaactions aNaw + average aOldTransactions aNaw) `div` toInteger aSize

                aFilter :: Seq (Transaction, TimeSpec) -> Seq (Transaction, TimeSpec)
                aFilter = S.filter (\s -> average' aNaw s < aAverage)

            if  | aSize < 500                  -> do
                    writeLog aInfoChan [PendingTag] Info "Pending size < 500"
                    loop $ Pending
                        (aNewTransaactions :|> (aTransaction, aNaw))
                        aOldTransactions
                | aSizeOfOldTransactions > 400 -> do
                    writeLog aInfoChan [PendingTag] Info "A sizi of old transaction < 400"
                    loop $ Pending
                        (aNewTransaactions :|> (aTransaction, aNaw))
                        (aFilter aOldTransactions)
                | otherwise                    -> do
                    writeLog aInfoChan [PendingTag] Info "Clearing of pending"
                    loop $ Pending
                        (aFilter aNewTransaactions :|> (aTransaction, aNaw))
                        (aFilter aOldTransactions)
{-
        -- transactions cleaning by the reason of including to block

        RemoveTransactions  aTransactions           -> do
            writeLog aInfoChan [PendingTag] Info $ "Remove transactions from pending. From pendig."
            let aFilter = S.filter (\(t, _) -> t `notElem` aTransactions)
            loop $ Pending (aFilter aNewTransaactions) (aFilter aOldTransactions)
-}
        -- transactions request

        GetTransaction      aCount aResponseChan    -> do
            writeLog aInfoChan [PendingTag] Info $ "Request " ++ show aCount ++ " transactions from pending"
            let aSizeOfOldTransactions = S.length aOldTransactions
                aSizeOfNewTransactions = S.length aNewTransaactions
                aSize = aSizeOfNewTransactions + aSizeOfOldTransactions
                -- if there are a lot of transactions
            if  | aCount < aSizeOfNewTransactions -> do
                    -- send n new transactions and replace it to "old"
                    let (aHead, aTail) = S.splitAt aCount aNewTransaactions
                    writeChan aResponseChan $ fst <$> toList aHead
                    loop $ Pending aTail (aOldTransactions >< aHead)


                | otherwise -> do
                    -- take from "old" needed count of txs
                    -- add new txs from "new" to "old"
                    -- used "old" txs and put it to the end
                    let (aHead, aTail) = S.splitAt (aCount - aSizeOfNewTransactions) aOldTransactions
                    writeChan aResponseChan $ fst <$> (toList $ aNewTransaactions >< aHead)
                    loop $ Pending Empty (aTail >< aNewTransaactions >< aHead)

-- at first

average :: Seq (Transaction, TimeSpec) -> TimeSpec -> Integer
average aTransactions aTime = sum $ average' aTime <$> aTransactions


average' :: TimeSpec -> (Transaction, TimeSpec) -> Integer
average' aTime (aTr, aT) = aTimeDiff `div` aAmount
  where
    -- using average
    aAmount   = toInteger (fromEnum (log $ toEnum $ fromEnum $ _amount aTr :: Double)) + 1
    aTimeDiff = toNanoSecs $ diffTimeSpec aTime aT


-- sum ((time - now) / log mount)
-- delete transactions depends on priority
-- delete transactions depends on their "age"
-- if time / log mount then time * log mount

-- do we have the same transaction?
-- do we have a space for a new transaction?


--------------------------------------------------------------------------------

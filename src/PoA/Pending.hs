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
-- actor
-- actor's data
-- commands for actor
--
-- add transaction
-- get n transaction (or less than n)
-- delete transaction from microblock
-- get rid of transactions (out of memory)

-- если транзакций много, то запускать функцию очистки, если их не много, то и фиг с ними.
-- определение памяти (пока 1k транзакций в пендинге)
-- pending structure () структура пендинга 2 очериди.

-- структура dupChan (микроблок чан)
-- get from transaction chan and pack into the message of another type
-- Block -> RemoveTransactions [Transactions]
-- Transaction -> AddTransaction Transaction


data PendingAction where
    RemoveTransactions  :: [Transaction]           -> PendingAction
    AddTransaction      :: Transaction              -> PendingAction
    GetTransaction      :: Int -> Chan [Transaction]-> PendingAction


data Pending = Pending (Seq (Transaction, TimeSpec)) (Seq (Transaction, TimeSpec))


pendingActor :: Chan PendingAction -> Chan Microblock -> Chan Transaction -> IO ()
pendingActor aChan aMicroblockChan aTransactionChan = do

    void . forkIO $ do
        aBlockChan <- dupChan aMicroblockChan
        -- перепаковка блоков
        forever $ readChan aBlockChan >>= \case
            Microblock _ _ _ _ aTransactions _ ->
                writeChan aChan $ RemoveTransactions aTransactions

    -- перепаковка транзакций
    void . forkIO $ forever $ forever $ readChan aTransactionChan >>=
        writeChan aChan . AddTransaction

    -- основное тело актора
    void $ loop $ Pending Empty Empty
  where
    loop (Pending aNewTransaactions aOldTransactions) = readChan aChan >>= \case
        AddTransaction aTransaction -> do
            aNaw <- getTime Realtime
            let aSizeOfOldTransactions = S.length aOldTransactions
                aSizeOfNewTransactions = S.length aNewTransaactions
                aSize = aSizeOfNewTransactions + aSizeOfOldTransactions
                aAverage = (average aNewTransaactions aNaw + average aOldTransactions aNaw) `div` toInteger aSize

                aFilter :: Seq (Transaction, TimeSpec) -> Seq (Transaction, TimeSpec)
                aFilter = S.filter (\s -> average' aNaw s < aAverage)

            if  | aSize < 500                  ->
                    loop $ Pending
                        (aNewTransaactions :|> (aTransaction, aNaw))
                        aOldTransactions
                | aSizeOfOldTransactions > 400 ->
                    loop $ Pending
                        (aNewTransaactions :|> (aTransaction, aNaw))
                        (aFilter aOldTransactions)
                | otherwise                    ->
                    loop $ Pending
                        (aFilter aNewTransaactions :|> (aTransaction, aNaw))
                        (aFilter aOldTransactions)

        -- Чистка транзакций по признаку вхождения в блок
        RemoveTransactions  aTransactions           -> do
            let aFilter = S.filter (\(t, _) -> t `notElem` aTransactions)
            loop $ Pending (aFilter aNewTransaactions) (aFilter aOldTransactions)

        -- запрос транзакции
        GetTransaction      aCount aResponseChan    -> do
            let aSizeOfOldTransactions = S.length aOldTransactions
                aSizeOfNewTransactions = S.length aNewTransaactions
                aSize = aSizeOfNewTransactions + aSizeOfOldTransactions
                -- если есть достаточно новых транзакций
            if  | aCount < aSizeOfNewTransactions -> do
                    -- отправить n новых транзакций и переложить их в старые
                    let (aHead, aTail) = S.splitAt aCount aNewTransaactions
                    writeChan aResponseChan $ fst <$> toList aHead
                    loop $ Pending aTail (aOldTransactions >< aHead)

                -- иначе
                | otherwise -> do
                    -- взять из старых недостающее число транзакций
                    -- добавить в старые новые
                    -- взятые использованые старые и переложить в конец
                    let (aHead, aTail) = S.splitAt (aCount - aSizeOfNewTransactions) aOldTransactions
                    writeChan aResponseChan $ fst <$> (toList $ aNewTransaactions >< aHead)
                    loop $ Pending Empty (aTail >< aNewTransaactions >< aHead)

-- в первую очереь

average :: Seq (Transaction, TimeSpec) -> TimeSpec -> Integer
average aTransactions aTime = sum $ average' aTime <$> aTransactions


average' :: TimeSpec -> (Transaction, TimeSpec) -> Integer
average' aTime (aTr, aT) = aTimeDiff `div` aAmount
  where
    -- используем усреднённый показатель
    aAmount   = toInteger (fromEnum (log $ toEnum $ fromEnum $ getAmount aTr :: Double)) + 1
    aTimeDiff = toNanoSecs $ diffTimeSpec aTime aT


getAmount :: Transaction -> Amount
getAmount = \case
    WithTime _ aTransaction            -> getAmount aTransaction
    WithSignature aTransaction _       -> getAmount aTransaction
    RegisterPublicKey _ aAmount        -> aAmount
    SendAmountFromKeyToKey _ _ aAmount -> aAmount


-- sum ((time - now) / log mount)
-- удаление транзакций в зависимости от приоритета
-- удаление транзакций в зависимости от возраста
-- if time / log mount then time * log mount

-- а есть ли у нас уже такая транзакция?
-- а есть ли у нас место под новую транзакцию?


--------------------------------------------------------------------------------

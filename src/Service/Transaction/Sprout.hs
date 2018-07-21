{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Sprout where

import           Control.Exception
import           Control.Monad
import qualified Data.HashTable.IO                as H
import           Data.Maybe
import qualified Data.Serialize                   as S (decode, encode)
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types

sproutTable :: IO SproutTable
sproutTable = sproutT
  where v1 = Just (read "1" :: HashOfKeyBlock)
        v2 = Just (read "2" :: HashOfKeyBlock)
        v3 = Just (read "3" :: HashOfKeyBlock)
        v11 = Just (read "11" :: HashOfKeyBlock)
        v22 = Just (read "22" :: HashOfKeyBlock)
        v33 = Just (read "33" :: HashOfKeyBlock)
        kv = [(1, (v1,v11)), (2, (v2,v22)), (3, (v3,v33))]
        sproutT = H.fromList kv


-- -- H.lookup ht $ key
-- -- H.insert ht key value

getChain :: Common -> Number -> IO Chain
getChain (Common descr _) number = do
  -- maybeV <- H.lookup st $ number
  maybeV <- funR (poolSprout descr) (S.encode number)
  case maybeV of
    Nothing    -> return (Nothing, Nothing)
    Just m -> case S.decode m :: Either String Chain of
      Left e  -> throw (DecodeException (show e))
      Right r -> return r


findChain :: Common -> Number -> BranchOfChain -> IO (Number, Maybe HashOfKeyBlock)
findChain c number branch = do
  let fun = case branch of
        Main   -> fst
        Sprout -> snd
  chain <- fun <$> getChain c number
  return (number, chain)


getM :: Common -> Number -> IO (Maybe HashOfKeyBlock)
getM c number = do
  chain <- getChain c number
  return $ fst chain



setS :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setS c@(Common descr _) number hashOfKeyBlock branch = when (branch == Sprout) $ do
  chain <- getChain c number
  let valueOfChain = funBranch branch $ chain
  let newChain = if (valueOfChain == Nothing)
        then case branch of
        Main   -> (Just hashOfKeyBlock, snd chain)
        Sprout -> (fst chain, Just hashOfKeyBlock)
        else throw (ValueOfChainIsNotNothing ("KeyBlockHash is" ++ (show valueOfChain)))

  let key = S.encode number
      val = S.encode newChain
  funW (poolSprout descr) [(key, val)]


funBranch :: BranchOfChain -> (a, a) -> a
funBranch Main   = fst
funBranch Sprout = snd


findWholeChainSince ::  Common -> Number -> BranchOfChain -> IO [(Number, HashOfKeyBlock)]
findWholeChainSince c number branch = do
  chainJ <- findChain c number branch
  let second = \a -> isJust $ snd a
  if (second chainJ)
    then do
    rest <- findWholeChainSince c (number + 1) branch
    let chain = (fst chainJ, fromJust (snd chainJ))
    return (chain:rest)
    else return []


findConsequentChainSinceUntil :: Common -> HashOfKeyBlock -> HashOfKeyBlock -> Limit -> IO [(HashOfKeyBlock, Number)]
findConsequentChainSinceUntil c@(Common descr i) h searchedHash limit = do
  macroblockMaybe <- getKeyBlockByHash descr (Hash h) i
  case macroblockMaybe of
    Nothing -> return []
    Just macroblock -> case _prevHKBlock macroblock of
      Nothing -> return []
      Just hashPrevKBlock -> if (limit > 0 && hashPrevKBlock /= searchedHash)
        then do
        let number = _number (macroblock :: MacroblockBD)
        rest <- findConsequentChainSinceUntil c hashPrevKBlock searchedHash (limit - 1)
        return ((hashPrevKBlock,number):rest)
      else return []


bbb :: IO [(Int, Maybe Int)]
bbb = return [(1, Just 1), (2, Just 2), (25, Just 25), (3, Nothing), (4, Just 4)]


pickByNumber :: Int -> IO (Int, Maybe Int)
pickByNumber i = do
  c <- bbb
  return $ c !! i


findSinceAndUntil :: Int -> IO [(Int, Maybe Int)]
findSinceAndUntil i = do
  p <- pickByNumber i
  let second = \a -> isJust $ snd a
  if (second p)
    then do
    rest <- findSinceAndUntil (i+1)
    return (p:rest)
    else return []

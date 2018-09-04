{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Legacy.Service.Transaction.Sprout where

import           Data.Maybe
import           Enecuum.Legacy.Service.Sync.SyncTypes
import           Enecuum.Legacy.Service.Transaction.Decode
import           Enecuum.Legacy.Service.Types
import           Prelude                                   ((!!))
import           Universum


findChain :: Common -> Number -> BranchOfChain -> IO (Number, Maybe HashOfKeyBlock)
findChain c aNumber branch = do
  let fun = case branch of
        Main   -> fst
        Sprout -> snd
  chain <- fun <$> getChain c aNumber
  return (aNumber, chain)


getM :: Common -> Number -> IO (Maybe HashOfKeyBlock)
getM c aNumber = fst <$> getChain c aNumber


findWholeChainSince ::  Common -> Number -> BranchOfChain -> IO [(Number, HashOfKeyBlock)]
findWholeChainSince c aNumber branch = do
  chainJ <- findChain c aNumber branch
  let second a = isJust $ snd a
  if second chainJ
    then do
    rest <- findWholeChainSince c (aNumber + 1) branch
    let chain = (fst chainJ, fromJust (snd chainJ))
    return (chain:rest)
    else return []


findConsequentChainSinceUntil :: Common -> HashOfKeyBlock -> HashOfKeyBlock -> Limit -> IO [(HashOfKeyBlock, Number)]
findConsequentChainSinceUntil c h searchedHash limit = do
  macroblockMaybe <- getKeyBlockByHash c (Hash h)
  case macroblockMaybe of
    Nothing -> return []
    Just macroblock -> case _prevHKBlock macroblock of
      Nothing -> return []
      Just hashPrevKBlock -> if limit > 0 && hashPrevKBlock /= searchedHash
        then do
        let aNumber = _number (macroblock :: MacroblockBD)
        rest <- findConsequentChainSinceUntil c hashPrevKBlock searchedHash (limit - 1)
        return ((hashPrevKBlock,aNumber):rest)
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
  let second a = isJust $ snd a
  if second p
    then do
    rest <- findSinceAndUntil (i+1)
    return (p:rest)
    else return []

{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Sprout where

import           Control.Exception
import qualified Data.HashTable.IO                as H
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
getChain (Common descr _ _) number = do
  -- maybeV <- H.lookup st $ number
  maybeV <- funR (poolSprout descr) (S.encode number)
  case maybeV of
    Nothing    -> return (Nothing, Nothing)
    Just m -> case S.decode m :: Either String Chain of
      Left e  -> throw (DecodeException (show e))
      Right r -> return r


findChain :: Common -> (Number, HashOfKeyBlock) -> BranchOfChain -> IO (Number, Maybe HashOfKeyBlock)
findChain c (number, hashofkeyblock) branch = do
  let fun = case branch of
        Main   -> fst
        Sprout -> snd
  chain <- fun <$> getChain c number
  return (number, chain)


getM :: Common -> Number -> IO (Maybe HashOfKeyBlock)
getM c number = do
  chain <- getChain c number
  return $ fst chain



setS :: Common -> Number -> HashOfKeyBlock -> IO ()
setS c@(Common descr _ _) number hashOfKeyBlock = do
  chain <- getChain c number
  case chain of
    (_, Just j) -> do
      let err k = "There is sprout already in the table, hashOfKeyBlock " ++ show k
      throw (SproutExists (err j))
    (m, Nothing) -> do
      let key = S.encode number
          val = S.encode (m, Just hashOfKeyBlock)
      funW (poolSprout descr) [(key, val)]

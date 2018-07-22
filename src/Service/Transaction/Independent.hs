{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Independent where
import           Control.Exception
import           Control.Monad
import qualified Data.Serialize                   as S (decode, encode)
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types



getChain :: Common -> Number -> IO Chain
getChain (Common descr _ ) aNumber = do
  maybeV <- funR (poolSprout descr) (S.encode aNumber)
  case maybeV of
    Nothing    -> return (Nothing, Nothing)
    Just m -> case S.decode m :: Either String Chain of
      Left e  -> throw (DecodeException (show e))
      Right r -> return r

setChain :: Common -> Number -> HashOfKeyBlock -> BranchOfChain -> IO ()
setChain c@(Common descr _ ) aNumber hashOfKeyBlock branch = when (branch == Sprout) $ do
  chain <- getChain c aNumber
  let valueOfChain = funBranch branch $ chain
  let newChain = if (valueOfChain == Nothing)
        then case branch of
        Main   -> (Just hashOfKeyBlock, snd chain)
        Sprout -> (fst chain, Just hashOfKeyBlock)
        else throw (ValueOfChainIsNotNothing ("KeyBlockHash is" ++ (show valueOfChain)))

  let key = S.encode aNumber
      val = S.encode newChain
  funW (poolSprout descr) [(key, val)]


funBranch :: BranchOfChain -> (a, a) -> a
funBranch Main   = fst
funBranch Sprout = snd

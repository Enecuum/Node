{-#LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings, MultiParamTypeClasses, ScopedTypeVariables #-}
module Service.HammingDistance where

import qualified    Data.ByteString as B
import              Data.Bits
import              Node.Crypto
import              Data.Word
import              Data.Serialize
import              Control.Monad
import              Data.List.Extra
import              System.Random.Shuffle

-- побитовая свёртка
class BitFoldable a where
    bitFold :: (b -> Bool -> b) -> b -> a -> b


instance FiniteBits a => BitFoldable a where
    bitFold f s a = foldl f s [testBit a x|x <- [0..finiteBitSize a -1]]


instance {-# OVERLAPS #-} BitFoldable a => BitFoldable [a] where
    bitFold f = foldl (bitFold f)


class Distancable a b where
    distance :: a -> b -> Int


instance (Serialize a, Serialize b) => Distancable a b where
    distance a b = distance (cryptoHash a) (cryptoHash b)


instance {-# OVERLAPS #-} Distancable B.ByteString B.ByteString where
    distance a b = aLengthDiverse + aDiverseBits
      where
        aDiverseBits :: Int
        aDiverseBits = bitFold (\i s -> fromEnum s + i) 0 $
            zipWith xor (B.unpack a) (B.unpack b)

        aLengthDiverse :: Int
        aLengthDiverse = 8 * abs ((B.length a) - (B.length b))
class SimpleDistance a where
    simpleDistance :: a -> a -> Word64


instance SimpleDistance [Word64] where
    {-# INLINE simpleDistance #-}
    simpleDistance a b = maximum $ zipWith dist a b
      where
        {-# INLINE dist #-}
        dist :: Word64 -> Word64 -> Word64
        dist x y = let d = x - y in min d (-d)

instance SimpleDistance B.ByteString where
    simpleDistance a b = simpleDistance (toHash a) (toHash b)

toHash :: B.ByteString -> [Word64]
toHash s = case runGet (forM [1..8 :: Int] $ \_ -> getWord64be) s of
    Right x -> x
    _ -> error "toHash"

--------------------------------------------------------------------------------

data TestNode = TestNode {
    testNodeId   :: B.ByteString,
    testNodeIds  :: [B.ByteString]
  }

testNet1 :: IO ()
testNet1 = do
    aNodeIdFile   <- B.readFile "./data/keys/genKeyFile0.bin"
    putStrLn "file 1"
    aNodeDataFile <- B.readFile "./data/keys/genKeyFile3.bin"
    putStrLn "file 2"
    let Right (aNodeIds  :: [B.ByteString]) = decode aNodeIdFile
        Right (aNodeData :: [B.ByteString]) = decode aNodeDataFile
    print $ minimum $ [simpleDistance a b| b <- take 30 $ toHash <$> aNodeIds, a <- take 1000 $ toHash <$> aNodeData, a /= b]
    print $ maximum $ [simpleDistance a b| b <- take 30 $ toHash <$> aNodeIds, a <- take 1000 $ toHash <$> aNodeData, a /= b]
    print $ length <$> filter (< 118) <$> [[simpleDistance a b | a <- take 30000 $ toHash <$> aNodeData, a /= b]| b <- take 200 $ toHash <$> aNodeIds]
    --print $ maximum $ length <$> filter (< 6000) <$> [[simpleDistance a b | a <- take 1000 $ aNodeData, a /= b]| b <- take 30 $ aNodeIds]

genKeyFile :: IO ()
genKeyFile = forM_ [0..99] $ \i -> do
    let keys = cryptoHash <$> [i*100000+1..100000+i*100000 :: Int]
    B.writeFile ("./data/keys/genKeyFile" ++ show i ++".bin") (encode keys)
    putStrLn $ "File: " ++ show i ++ " is writed."



nodeD :: TestNode -> Word64
nodeD aNode = sum (simpleDistance (testNodeId aNode) <$> testNodeIds aNode)


electNodeIds :: [B.ByteString] -> B.ByteString -> IO [B.ByteString]
electNodeIds aList aId = do
    aListShuffled <- shuffleM aList
    let randList = filter (aId /=) $ take 200 aListShuffled
    return $ take 1 $ sortOn (simpleDistance aId) randList


makeNodeList :: Int -> [B.ByteString] -> IO [TestNode]
makeNodeList aN aNodeIds = do
    let takedNodes = take aN aNodeIds
    forM takedNodes $ \aId -> do
        aElNodeIds <- electNodeIds takedNodes aId
        let aNode = TestNode aId aElNodeIds
        return $ aNode

dataState :: [TestNode] -> [B.ByteString] -> IO ()
dataState aNodeList aNodeData =
    print $ length <$> do
        aNode <- aNodeList
        pure  $ do
            let d = nodeD aNode
            aData <- aNodeData
            guard $ simpleDistance (testNodeId aNode) aData < d
            return True

testNet2 :: IO ()
testNet2 = do
    aNodeIdFile   <- B.readFile "./data/keys/genKeyFile0.bin"
    putStrLn "file 1"
    aNodeDataFile <- B.readFile "./data/keys/genKeyFile3.bin"
    putStrLn "file 2"
    let Right (aNodeIds  :: [B.ByteString]) = decode aNodeIdFile
        Right (aNodeData :: [B.ByteString]) = decode aNodeDataFile
        aNumOfNode = 300
    aTestNet <- makeNodeList aNumOfNode aNodeIds
    print $ sum (nodeD <$> aTestNet) `div` aNumOfNode
    dataState aTestNet (take 300 $ aNodeData)
-}

testNet3 :: IO ()
testNet3 = do
    aNodeIdFile   <- B.readFile "./data/keys/genKeyFile0.bin"
    putStrLn "file 1"
    aNodeDataFile <- B.readFile "./data/keys/genKeyFile3.bin"
    putStrLn "file 2"
    let Right (aNodeIds  :: [B.ByteString]) = decode aNodeIdFile
        Right (aNodeData :: [B.ByteString]) = decode aNodeDataFile
        aBlocks = take aNumOfBlocks $ toHash <$> aNodeData
        aNodes  = take aNumOfNode   $ toHash <$> aNodeIds
        sizeOfBuff   = 100
        aNumOfNode   = 100
        aNumOfBlocks = 10000
    forM_ aBlocks $ \aBlock -> do
        if any (aBlock `elem`)
            ((\aId -> take sizeOfBuff $ sortOn (simpleDistance aId) aBlocks) <$> aNodes)
        then putStrLn $ "Ok. "
        else putStrLn $ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

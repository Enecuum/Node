module Node.DBActor where


data MsgToDB where
    KeyBlockMsgToDB :: Value -> MsgToDB
    MicroblockMsgToDB :: Microblock -> MsgToDB

    MyTail :: MVar (HashOfKeyBlock, Number) -> MsgToDB
    GetHashOfLastClosedKeyBlock :: MVar (HashOfKeyBlock, Number) -> MsgToDB
    PeekNPreviousClosedKeyBlocks :: Int -> HashOfKeyBlock -> MVar [(HashOfKeyBlock, Number)] -> MsgToDB

    GetKeyBlockSproutData :: From -> To -> MVar [MacroblockBD]-> MsgToDB
    SetKeyBlockSproutData :: [MacroblockBD] -> MVar Bool -> MsgToDB

    GetRestSproutData :: [MickroBlockHash] -> MVar MicroBlockContent -> MsgToDB
    SetRestSproutData :: [MicroBlockContent] -> MVar Bool -> MsgToDB

resender = void . C.forkIO . forever . writeInChan

microblockProc descriptor aMicroblockCh aValueChan aInfoCh (aInChan, aOutChan) = do
    resender aInChan . MicroblockMsgToDB =<< readChan aMicroblockCh
    resender aInChan . KeyBlockMsgToDB =<< readChan aValueChan
    forever $ readChan aOutChan >>= \case
        MicroblockMsgToDB aMicroblock ->
            addMicroblockToDB descriptor aMicroblock aInfoCh

        KeyBlockMsgToDB aValue ->
            addMacroblockToDB descriptor aValue aInfoCh

        MyTail aMVar ->
            putMVar aMVar =<< myTail descriptor aInfoCh

        GetHashOfLastClosedKeyBlock aMVar ->
            putMVar aMVar =<< getHashOfLastClosedKeyBlock descriptor aInfoCh

        PeekNPreviousClosedKeyBlocks aInt aHashOfKeyBlock aMVar ->
            putMVar aMVar =<< peekNPreviousClosedKeyBlocks descriptor aInfoCh aInt aHashOfKeyBlock

        GetKeyBlockSproutData aFrom aTo aMVar -> do
            putMVar aMVar =<< getKeyBlockSproutData descriptor aInfoCh aFrom aTo

        SetKeyBlockSproutData aMacroblockBD aMVar -> do
            aIsValid <- isValidKeyBlockSprout descriptor aInfoCh aMacroblockBD
            when aIsValid setKeyBlockSproutData descriptor aInfoCh aMacroblockBD
            putMVar aMVar aIsValid

        GetRestSproutData aMickroBlockHash aMVar -> do
            putMVar aMVar =<< getKeyBlockSproutData descriptor aInfoCh aMickroBlockHash

        SetRestSproutData aMicroBlockContent aMVar -> do
            aIsValid <- isValidRestOfSprout descriptor aInfoCh aMicroBlockContent
            when aIsValid setRestSproutData descriptor aInfoCh aMicroBlockContent
            putMVar aMVar aIsValid


myTail :: DBPoolDescriptor -> InChan InfoMsg -> IO Number
myTail = undefined

getHashOfLastClosedKeyBlock ::  DBPoolDescriptor -> InChan InfoMsg -> IO (HashOfKeyBlock, Number)
getHashOfLastClosedKeyBlock = undefined

peekNPreviousClosedKeyBlocks :: DBPoolDescriptor -> InChan InfoMsg -> Int -> HashOfKeyBlock -> IO [(HashOfKeyBlock, Number)]
peekNPreviousClosedKeyBlocks = undefined

setKeyBlockSproutData :: DBPoolDescriptor -> InChan InfoMsg -> [MacroblockBD] -> IO ()
setKeyBlockSproutData = undefined

getKeyBlockSproutData :: DBPoolDescriptor -> InChan InfoMsg -> From -> To -> IO [MacroblockBD]
getKeyBlockSproutData = undefined

getRestSproutData :: DBPoolDescriptor -> InChan InfoMsg -> [HashOfMicroblock] -> IO MicroBlockContent
getRestSproutData = undefined

setRestSproutData :: DBPoolDescriptor -> InChan InfoMsg -> [MicroBlockContent]  -> IO ()
setRestSproutData = undefined

isValidKeyBlockSprout :: DBPoolDescriptor -> InChan InfoMsg -> [MacroblockBD] -> IO Bool
isValidKeyBlockSprout = undefined

isValidRestOfSprout :: DBPoolDescriptor -> InChan InfoMsg -> MicroBlockContent -> IO Bool
isValidRestOfSprout = undefined

{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
module Node.Node.Config.Make where

import qualified    Data.ByteString as B
import              Data.Serialize
import              Data.Monoid
import              Control.Monad
import              Control.Monad.State.Lazy
import              Lens.Micro.Mtl
import              Control.Exception
import              Network.Socket
import              Node.Node.Types
import              System.Environment
import              Node.Data.Data
import              Service.System.Directory (createFilesDirectory)


makeFileConfig :: String -> NodeVariantRoles -> PortNumber -> IO ()
makeFileConfig path aRoles aPort = do
    nConfig <- makeNewNodeConfig aPort
    createFilesDirectory path
    B.writeFile path $ encode $ (execState $ do
        helloMsg.nodeVariantRoles   .= aRoles
        helloMsg.listenPort         .= aPort
        portNumber                  .= aPort
      ) nConfig


makeNodeConfigsForTestNets :: IO ()
makeNodeConfigsForTestNets = do
    forM_ [1..50] $ \i -> makeFileConfig
        ("./data/broadcastNodeConfigs/miningInitData" <> show i <> ".bin")
        [BroadcastNode] (1600+i)
    forM_ [1..100] $ \i -> makeFileConfig
        ("./data/simpleNodeConfigs/miningInitData" <> show i <> ".bin")
        [SimpleNode] (1700 + i)


readBootNodeList :: IO BootNodeList
readBootNodeList = do
    try (B.readFile "./data/bootNodeList.bin") >>= \case
        Right bootNodeList         -> case decode bootNodeList of
            Right bootNodeData      -> pure bootNodeData
            Left _                  -> pure []
        Left (_ :: SomeException)   -> do
            try (readFile "./data/bootNodeList.conf") >>= \case
                Right bootNodeList -> do
                    try (return $ read bootNodeList) >>= \case
                        Right aList -> do
                            toNormForm aList
                        Left (_ :: SomeException) -> do
                            return []
                Left (_ :: SomeException) -> do
                    aBootNodeListData <- try (getEnv "bootNodeList")
                    case aBootNodeListData of
                        Right bootNodeList -> do
                            toNormForm $ read bootNodeList
                        Left (_ :: SomeException) -> do
                            return []
   where
     toNormForm aList = return $ (\(a,b,c) -> (NodeId a,tupleToHostAddress b, c))
        <$> aList

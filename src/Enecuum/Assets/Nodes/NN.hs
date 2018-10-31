{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Enecuum.Assets.Nodes.NN where

import           Enecuum.Prelude
import qualified Data.Aeson                     as J
import qualified Enecuum.Domain                 as D
import qualified Enecuum.Language               as L
import qualified Enecuum.Assets.Nodes.Address   as A
import qualified Enecuum.Assets.Nodes.Messages  as M
import           Enecuum.Research.ChordRouteMap
import           Enecuum.Framework.Language.Extra (HasStatus)

data NNNodeData = NNNodeData
    { _status   :: D.StateVar L.NodeStatus
    , _netNodes :: D.StateVar (ChordRouteMap D.Address)
    , _nodePort :: D.StateVar (Maybe D.PortNumber)
    }
makeFieldsNoPrefix ''NNNodeData

newtype Start = Start D.PortNumber

instance J.FromJSON Start where
    parseJSON = J.withObject "Start" $ \o -> Start <$> o J..: "port"

startNode :: NNNodeData -> Start -> L.NodeL Text
startNode nodeData (Start port) = L.atomically $ do
    currentPort <- L.readVar (nodeData^.nodePort)
    unless (isJust currentPort) $
        L.writeVar (nodeData^.nodePort) $ Just port
    pure $ if isJust currentPort
        then "Node is already running."
        else "The port is accepted, the node is started."

initNN :: Maybe D.PortNumber -> L.NodeDefinitionL NNNodeData
initNN maybePort = L.atomically
    (NNNodeData <$> L.newVar L.NodeActing <*> L.newVar mempty <*> L.newVar maybePort)

awaitPort :: NNNodeData -> L.NodeDefinitionL D.PortNumber
awaitPort nodeData = L.atomically $ do
    currentPort <- L.readVar $ nodeData ^. nodePort
    case currentPort of
        Just port -> pure port
        Nothing   -> L.retry

connectToBN :: D.Address -> D.Address -> NNNodeData -> L.NodeDefinitionL ()
connectToBN myAddress bnAddress nodeData = do
    let hash = D.toHashGeneric myAddress
    -- TODO add proccesing of error
    _ :: Either Text M.SuccessMsg <- L.scenario $ L.makeRpcRequest bnAddress $ M.Hello hash myAddress
    let loop i = when (i > 0) $ do
            maybeAddress :: Either Text (D.StringHash, D.Address) <- L.scenario $ L.makeRpcRequest bnAddress $ M.ConnectRequest hash i
            case maybeAddress of
                Right (recivedHash, address) | myAddress /= address -> do
                    L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap recivedHash address)
                    loop (i - 1)
                _ -> pure ()
    loop (hashSize - 1)
    maybeAddress :: Either Text (D.StringHash, D.Address) <- L.scenario $ L.makeRpcRequest bnAddress $ M.ConnectRequestPrevious hash
    case maybeAddress of
        Right (_, address) | myAddress /= address ->
            void $ L.notify address $ M.Hello hash myAddress
        _ -> pure ()

acceptHello :: NNNodeData -> M.Hello -> D.Connection D.Udp -> L.NodeL ()
acceptHello nodeData (M.Hello hash address) con = do
    L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap hash address)
    L.close con

acceptConnectResponse :: NNNodeData -> D.Address -> M.ConnectResponse -> D.Connection D.Udp -> L.NodeL ()
acceptConnectResponse nodeData myAddress (M.ConnectResponse hash address) con = do
    when (myAddress /= address) $
        L.atomically $ L.modifyVar (nodeData ^. netNodes) (addToMap hash address)
    L.close con

acceptConnectRequest :: NNNodeData -> M.ConnectRequest -> D.Connection D.Udp -> L.NodeL ()
acceptConnectRequest nodeData (M.ConnectRequest hash i) conn = do
    maybeAddress <- L.atomically $ do
        connectMap <- L.readVar (nodeData ^. netNodes)
        pure $ findInMapNByKey
            (\h num -> (D.hashToInteger h + 2 ^ num) `mod` quantityOfHashes)
            i
            hash
            connectMap
    whenJust maybeAddress $ \(h, address) ->
        void $ L.send conn $ M.ConnectResponse h address
    L.close conn

stabilizationOfConnections :: D.Address -> NNNodeData -> L.NodeL ()
stabilizationOfConnections myAddress nodeData = do
    let hash = D.toHashGeneric myAddress
    connectMap <- L.atomically $ do
        nodes <- L.readVar (nodeData ^. netNodes)
        let filteredNodes   = findInMap hash nodes
        let filteredNodeMap = toChordRouteMap filteredNodes
        L.writeVar (nodeData ^. netNodes) filteredNodeMap
        pure $ snd <$> filteredNodes

    forM_ (zip connectMap [255, 254..]) $ \(addr, i) -> void $
        L.notify addr $ M.ConnectRequest hash i

acceptSendTo
    :: NNNodeData -> D.StringHash -> M.SendTo -> D.Connection D.Udp -> L.NodeL ()
acceptSendTo nodeData myHash (M.SendTo hash i msg) conn = do
    L.close conn
    L.logInfo $ "Recived msg: \"" <>  msg <> "\" for " <> show hash
    when (myHash == hash) $ L.logInfo "I'm reciver."
    when (i > 0) $ do
        rm <- L.readVarIO (nodeData ^. netNodes)
        whenJust (findNext hash rm) $ \(h, address) -> do
            L.logInfo $ "Resending to: " <> show h
            void $ L.notify address (M.SendTo hash (i-1) msg)

nnNode :: Maybe D.PortNumber -> L.NodeDefinitionL ()
nnNode maybePort = do
    L.nodeTag "NN node"
    L.logInfo "Starting of NN node"
    nodeData    <- initNN maybePort
    L.std $ do
        L.stdHandler $ startNode nodeData
        L.stdHandler $ L.stopNodeHandler nodeData

    port        <- awaitPort nodeData
    let myAddress = D.Address "127.0.0.1" port
    connectToBN myAddress A.bnAddress nodeData

    L.serving D.Udp port $ do
        L.handler $ acceptHello           nodeData
        L.handler $ acceptConnectResponse nodeData myAddress
        L.handler $ acceptConnectRequest  nodeData
        L.handler $ acceptSendTo          nodeData  (D.toHashGeneric myAddress)

    L.process $ forever $ do
        L.delay $ 1000 * 1000
        stabilizationOfConnections myAddress nodeData

    L.awaitNodeFinished nodeData
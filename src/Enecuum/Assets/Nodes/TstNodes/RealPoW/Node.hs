{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.RealPoW.Node
  ( TstRealPoWNode
  ) where

import           Control.Lens                                 (to)
import qualified Crypto.Hash.SHA256                           as SHA
import qualified Data.Aeson                                   as J
import qualified Data.Bits                                    as Bit
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Base64                       as Base64
import qualified Data.ByteString.Internal                     as BSI
import           Data.HGraph.StringHashable                   (StringHash (..), toHash)
import qualified Data.Serialize.Put                           as P

import qualified Enecuum.Assets.Nodes.Address                 as A
import qualified Enecuum.Assets.Nodes.Messages                as Msgs
import           Enecuum.Assets.Nodes.Methods
import           Enecuum.Assets.Nodes.TstNodes.RealPoW.Config as Tst
import           Enecuum.Blockchain.Lens                      as Lens
import           Enecuum.Config
import qualified Enecuum.Domain                               as D
import           Enecuum.Framework.Language.Extra             (HasStatus)
import qualified Enecuum.Language                             as L
import           Enecuum.Prelude

data KBlockTemplate = KBlockTemplate
    { _time     :: D.BlockTime
    , _number   :: D.BlockNumber
    , _nonce    :: D.Nonce
    , _prevHash :: D.RawHash       -- Not in base64
    , _solver   :: D.RawHash       -- Not in base64
    }

data KBlockDef = KBlockDef
    { _template   :: KBlockTemplate
    , _hash       :: D.RawHash     -- Not in base64
    , _difficulty :: D.Difficulty
    , _nonceRange :: D.NonceRange
    }

data TstRealPoWNodeData = TstRealPoWNodeData
    { _currentDifficulty :: D.StateVar D.Difficulty
    , _prevBlockDef      :: D.StateVar KBlockDef
    , _generated         :: D.SignalVar
    , _generatedBlockDef :: D.StateVar KBlockDef
    , _sendingBlock      :: D.StateVar (Maybe D.KBlock)
    , _status            :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''TstRealPoWNodeData
makeFieldsNoPrefix ''KBlockTemplate
makeFieldsNoPrefix ''KBlockDef

instance Node TstRealPoWNode where
    data NodeScenario TstRealPoWNode = TstRealPoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = TstRealPoWNode

instance ToJSON   (NodeScenario TstRealPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstRealPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

fromKBlockTemplate :: KBlockTemplate -> D.KBlock
fromKBlockTemplate KBlockTemplate {..} = D.KBlock
    { D._time     = _time
    , D._number   = _number
    , D._nonce    = _nonce
    , D._prevHash = D.fromRawHash _prevHash
    , D._solver   = D.fromRawHash _solver
    }

toKBlockTemplate :: D.KBlock -> KBlockTemplate
toKBlockTemplate kBlock = KBlockTemplate
    { _time     = kBlock ^. Lens.time
    , _number   = kBlock ^. Lens.number
    , _nonce    = kBlock ^. Lens.nonce
    , _prevHash = D.toRawHash $ kBlock ^. Lens.prevHash
    , _solver   = D.toRawHash $ kBlock ^. Lens.solver
    }

calcHash :: KBlockTemplate -> D.RawHash
calcHash KBlockTemplate {..} = D.calcKBlockHashRaw _time _number _nonce _prevHash _solver

generateHash
    :: D.Difficulty
    -> (D.Nonce -> KBlockTemplate)
    -> D.NonceRange
    -> Maybe KBlockDef
generateHash difficulty kBlockTemplateF range@(from, to) = do
    let difficulties = do
            kBlock' <- map kBlockTemplateF [from..to]
            let hash = calcHash kBlock'
            pure $ KBlockDef kBlock' hash (D.calcHashDifficulty hash) range

    let found = filter (\(KBlockDef _ _ dif _) -> dif > difficulty) difficulties
    case found of
      []      -> Nothing
      (res:_) -> Just res

generateKBlock
    :: TstRealPoWNodeData
    -> D.Difficulty
    -> (D.Nonce -> KBlockTemplate)
    -> D.NonceRange
    -> L.NodeL ()
generateKBlock nodeData difficulty kBlockTemplateF range = do
    let mbBlock = generateHash difficulty kBlockTemplateF range
    case mbBlock of
        Nothing        -> pure ()
        Just !blockDef -> L.atomically $ do
            L.writeVar (nodeData ^. generatedBlockDef) blockDef
            L.writeVar (nodeData ^. generated) True

kBlockGeneration :: TstRealPoWNodeData -> [D.NonceRange] -> L.NodeDefinitionL ()
kBlockGeneration nodeData ranges = do
    (difficulty, prevBlockDef') <- L.atomically $ (,)
        <$> L.readVar (nodeData ^. currentDifficulty)
        <*> L.readVar (nodeData ^. prevBlockDef     )

    time' :: D.BlockTime <- round <$> L.getPosixTime
    let nextNumber = 1 + prevBlockDef' ^. template . number
    let kBlockTemplateF n = KBlockTemplate
          { _time     = time'
          , _number   = nextNumber
          , _nonce    = n
          , _prevHash = prevBlockDef' ^. hash
          , _solver   = prevBlockDef' ^. template . solver
          }

    L.logInfo $ "Generating next KBlock (number: "+|| nextNumber ||+
        " , difficulty: " +|| difficulty ||+
        ")."

    generators <- mapM (L.fork . generateKBlock nodeData difficulty kBlockTemplateF) ranges

    L.awaitSignal $ nodeData ^. generated
    mapM_ L.killProcess generators

    L.atomically $ do
        blockDef@(KBlockDef blockTemplate hash _ range) <- L.readVar $ nodeData ^. generatedBlockDef
        let kBlock = fromKBlockTemplate blockTemplate

        L.writeVar (nodeData ^. generated) False
        L.writeVar (nodeData ^. sendingBlock) $ Just kBlock
        L.writeVar (nodeData ^. prevBlockDef) blockDef

        L.logInfo $ "Generated KBlock (" +|| D.fromRawHash hash ||+
            "). Number: " +|| kBlock ^. Lens.number ||+
            ", Nonce: "   +|| kBlock ^. Lens.nonce  ||+
            ", Range: "   +|| range ||+ "."

kBlockSending :: TstRealPoWNodeData -> D.Address -> L.NodeL ()
kBlockSending nodeData graphNodeUdpAddress = do
    kBlock <- L.atomically $ L.takeVar $ nodeData ^. sendingBlock

    L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    void $ L.withConnection D.Udp graphNodeUdpAddress $ \conn ->
        void $ L.send conn kBlock

initializeNode :: D.Difficulty -> L.NodeDefinitionL TstRealPoWNodeData
initializeNode difficulty = do
    let initialBlock = toKBlockTemplate D.genesisKBlock
    let initialBlockDef = KBlockDef
          { _template   = initialBlock
          , _difficulty = difficulty
          , _nonceRange = (0, 0)
          , _hash       = D.toRawHash D.genesisHash
          }

    currentDifficulty <- L.newVarIO difficulty
    prevBlockDef      <- L.newVarIO initialBlockDef
    generated         <- L.newVarIO False
    generatedBlockDef <- L.newVarIO initialBlockDef
    sendingBlock      <- L.newVarIO Nothing
    status            <- L.newVarIO D.NodeActing

    pure TstRealPoWNodeData
        { _currentDifficulty = currentDifficulty
        , _prevBlockDef      = prevBlockDef
        , _generated         = generated
        , _generatedBlockDef = generatedBlockDef
        , _sendingBlock      = sendingBlock
        , _status            = status
        }

-- TODO: https://task.enecuum.com/issues/2935
-- * Solver id
-- * Synchro
-- * What if no one found hash for this time && nonce?
-- * Difficulty func

powNode' :: NodeConfig TstRealPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "Tst Real PoW node"

    let workersCount = _workersCount cfg
    let difficulty   = _baseDifficulty cfg
    let window = (maxBound :: D.Nonce) `div` workersCount
    let ranges = [(i * window, (i + 1) * window) | i <- [0..workersCount - 1]]

    nodeData <- initializeNode difficulty

    L.serving D.Rpc (_controlRpcPort cfg) $ do
        L.method    rpcPingPong
        L.method  $ handleStopNode nodeData

    L.process $ forever $ kBlockSending nodeData (_graphNodeUDPAddress cfg)

    forever $ kBlockGeneration nodeData ranges

    L.awaitNodeFinished nodeData

{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Enecuum.Assets.Nodes.TstNodes.RealPoW.PoW
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

import qualified Enecuum.Assets.Blockchain.Generation         as A
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
    , _prevHash :: ByteString       -- Not in base64
    , _solver   :: ByteString       -- Not in base64
    }

data KBlockDef = KBlockDef
    { _template   :: KBlockTemplate
    , _hash       :: ByteString
    , _difficulty :: D.Difficulty
    }

data TstRealPoWNodeData = TstRealPoWNodeData
    { _currentDifficulty :: D.StateVar D.Difficulty
    , _prevBlock         :: D.StateVar KBlockTemplate
    , _generated         :: D.SignalVar
    , _generatedBlock    :: D.StateVar KBlockDef
    , _sendingBlock      :: D.StateVar (Maybe D.KBlock)
    , _status            :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''TstRealPoWNodeData

instance Node TstRealPoWNode where
    data NodeScenario TstRealPoWNode = TstRealPoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = TstRealPoWNode

instance ToJSON   (NodeScenario TstRealPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstRealPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

fromKBlockTempalte :: KBlockTemplate -> D.KBlock
fromKBlockTempalte KBlockTemplate {..} = D.KBlock
    { D._time     = _time
    , D._number   = _number
    , D._nonce    = _nonce
    , D._prevHash = D.StringHash $ Base64.encode _prevHash
    , D._solver   = D.StringHash $ Base64.encode _solver
    }

toKBlockTempalte :: D.KBlock -> KBlockTemplate
toKBlockTempalte kBlock = KBlockTemplate
    { _time     = kBlock ^. Lens.time
    , _number   = kBlock ^. Lens.number
    , _nonce    = kBlock ^. Lens.nonce
    , _prevHash = fromRight (error "Decoding hash from base64 failed.") $ Base64.decode $ kBlock ^. Lens.prevHash
    , _solver   = fromRight (error "Decoding hash from base64 failed.") $ Base64.decode $ kBlock ^. Lens.solver
    }

calcHash :: KBlockTemplate -> ByteString
calcHash KBlockTemplate {..} = D.calcKBlockHashRaw _time _number _nonce _prevHash _solver

generateHash
    :: D.Difficulty
    -> D.NonceRange
    -> Maybe KBlockDef
generateHash difficulty (from, to) = do
    let kBlockTemplate n = KBlockTemplate
          { _time     = 0
          , _number   = 1
          , _nonce    = n
          , _prevHash = fromRight (error "Decoding hash from base64 failed.") $ Base64.decode $ D.fromStringHash D.genesisHash
          , _solver   = fromRight (error "Decoding hash from base64 failed.") $ Base64.decode D.genesisSolverStr
          }

    let difficulties = do
            kBlock' <- map kBlockTemplate [from..to]
            let hash = calcHash kBlock'
            pure (kBlock', hash, D.calcHashDifficulty hash)

    let found = filter (\(_, _, dif) -> dif > difficulty) difficulties
    case found of
      []      -> Nothing
      (res:_) -> Just res

generateKBlock :: TstRealPoWNodeData -> D.Difficulty -> D.NonceRange -> L.NodeL ()
generateKBlock nodeData difficulty range = do
    let mbBlock = generateHash difficulty range
    case mbBlock of
        Nothing     -> pure Nothing
        Just !block -> L.atomically $ do
            L.writeVar (nodeData ^. generatedBlock) block
            L.writeVar (nodeData ^. generated) True

kBlockGeneration :: TstRealPoWNodeData -> [D.NonceRange] -> L.NodeDefinitionL ()
kBlockGeneration nodeData ranges = do
    difficulty <- L.readVarIO $ nodeData ^. currentDifficulty
    L.logInfo "Generating next KBlock, difficulty: " <> show difficulty

    generators <- mapM (L.fork . generateKBlock nodeData difficulty) ranges

    L.awaitSignal $ nodeData ^. generated
    mapM_ L.killProcess generators

    L.atomically $ do
        (blockTemplate, hash, dif) <- L.readVar $ nodeData ^. generatedBlock
        let kBlock = fromKBlockTemplate blockTemplate

        L.writeVar (nodeData ^. generated) False
        L.writeVar (nodeData ^. sendingBlock) $ Just kBlock
        L.writeVar (nodeData ^. prevBlock) blockTemplates

        L.logInfo $ "Generated KBlock (" +|| Base64.encode hash ||+ "): " <> show kBlock

kBlockSending :: TstRealPoWNodeData -> L.NodeDefinitionL ()
kBlockSending nodeData = do
    kBlock <- L.atomically $ L.takeVar $ nodeData ^. sendingBlock

    L.logInfo $ "\nSending KBlock (" +|| toHash kBlock ||+ "): " +|| kBlock ||+ "."
    L.withConnection D.Udp graphNodeUdpAddress $ \conn ->
        void $ L.send conn kBlock

initializeNode :: D.Difficulty -> L.NodeDefinitionL TstRealPoWNodeData
initializeNode difficulty = do
    let initialBlock = toKBlockTemplate D.genesisKBlock

    currentDifficulty <- L.newVarIO difficulty
    prevBlock         <- L.newVarIO initialBlock
    generated         <- L.newVarIO False
    generatedBlock    <- L.newVarIO (initialBlock, calcHash initialBlock, 0)
    sendingBlock      <- L.newVarIO Nothing
    status            <- L.newVarIO D.NodeActing

    pure TstRealPoWNodeData
        { _currentDifficulty = currentDifficulty
        , _prevBlock         = prevBlock
        , _generated         = generated
        , _generatedBlock    = generatedBlock
        , _sendingBlock      = sendingBlock
        , _status            = status
        }

powNode' :: NodeConfig TstRealPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "Tst Real PoW node"

    -- TODO: configs, difficulty formula
    let workersCount = 4
    let difficulty = 8
    let window = (maxBound :: D.Nonce) `div` workersCount
    let ranges = [(i * window, (i + 1) * window) | i <- [0..workersCount - 1]]

    nodeData <- initializeNode difficulty

    L.process $ forever $ kBlockSending nodeData

    forever $ kBlockGeneration nodeData ranges

    L.awaitNodeFinished nodeData

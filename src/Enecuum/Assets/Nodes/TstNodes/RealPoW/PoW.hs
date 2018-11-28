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
    , _prevKBlock        :: D.StateVar KBlockTemplate
    , _genProcessData    :: D.StateVar (Maybe KBlockDef)
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
    , D._prevHash = _prevHash
    , D._solver   = _solver
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
          , _prevHash = fromRight "" $ Base64.decode $ D.fromStringHash D.genesisHash
          , _solver   = fromRight "" $ Base64.decode D.genesisSolverStr
          }

    let difficulties = do
            kBlock' <- map kBlockTemplate [from..to]
            let hash = calcHash kBlock'
            pure (kBlock', hash, D.calcHashDifficulty hash)

    let found = filter (\(_, _, dif) -> dif > difficulty) difficulties
    case found of
      []      -> Nothing
      (res:_) -> Just res


processKBlock nodeData = do
    (kBlockTemplate, hash, difficulty) <- L.takeVar $ nodeData ^. kBlockDef

    let kBlock = fromKBlockTempalte kBlockTemplate
    L.logInfo $ "Generated block (difficulty: " +|| difficulty ||+ "): " <> show hash
    L.withConnection D.Udp graphNodeUdpAddress $ \conn ->
        void $ L.send conn kBlock

generateKBlock nodeData range = do
    difficulty <- L.readVarIO $ nodeData ^. currentDifficulty
    case generateHash difficulty range of
        Nothing     -> pure Nothing
        Just !found -> L.atomically $ do
            mbFoundByOthers <- L.readVar $ nodeData ^.


            let (KBlockDef kBlockTemplate _ _) = found
            L.writeVar (nodeData ^. prevKBlock) kBlockTemplate
            L.writeVar

kBlockGeneration nodeData ranges = do
    L.logInfo "Generating next KBlock..."

    generators <- mapM (L.fork . generateKBlock nodeData) ranges
    mapM_ L.awaitResult generators
    pure ()

powNode' :: NodeConfig TstRealPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "Tst Real PoW node"

    nodeData <- TstRealPoWNodeData <$> L.newVarIO D.NodeActing

    let workersCount = 4
    let difficulty = 8
    let window = (maxBound :: D.Nonce) `div` workersCount
    let ranges = [(i * window, (i + 1) * window) | i <- [0..workersCount - 1]]

    L.process $ forever $ processKBlock nodeData

    kBlockGeneration nodeData ranges

    L.awaitNodeFinished nodeData

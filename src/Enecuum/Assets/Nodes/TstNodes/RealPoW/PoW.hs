{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DeriveAnyClass         #-}
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

data TstRealPoWNodeData = TstRealPoWNodeData
    { _status :: D.StateVar D.NodeStatus
    }

makeFieldsNoPrefix ''TstRealPoWNodeData

instance Node TstRealPoWNode where
    data NodeScenario TstRealPoWNode = TstRealPoW
        deriving (Show, Generic)
    getNodeScript _ = powNode'
    getNodeTag _ = TstRealPoWNode

instance ToJSON   (NodeScenario TstRealPoWNode) where toJSON    = J.genericToJSON    nodeConfigJsonOptions
instance FromJSON (NodeScenario TstRealPoWNode) where parseJSON = J.genericParseJSON nodeConfigJsonOptions

data KBlockTemplate = KBlockTemplate
    { _time     :: D.BlockTime
    , _number   :: D.BlockNumber
    , _nonce    :: D.Nonce
    , _prevHash :: ByteString       -- Not in base64
    , _solver   :: ByteString       -- Not in base64
    }

calcHash :: KBlockTemplate -> ByteString
calcHash KBlockTemplate {..} = SHA.hash bstr
  where
    bstr = P.runPut $ do
          P.putWord8 (toEnum D.kBlockType)
          P.putWord32le   _number
          P.putWord32le   _time
          P.putWord32le   _nonce
          P.putByteString _prevHash
          P.putByteString _solver


generateHash
    :: TstRealPoWNodeData
    -> D.Difficulty
    -> D.NonceRange
    -> L.NodeDefinitionL ()
generateHash nodeData (fromIntegral -> difficulty) (from, to) = do
    let kBlockTemplate n = KBlockTemplate
          { _time     = 0
          , _number   = 1
          , _nonce    = n
          , _prevHash = fromRight "" $ Base64.decode $ D.fromStringHash D.genesisHash
          , _solver   = fromRight "" $ Base64.decode D.genesisSolverStr
          }
    let hashes = [ calcHash h | h <- map kBlockTemplate [from..to]]
    let difficulties = map D.calcHashDifficulty hashes

    mapM_ (L.logInfo . show) $ zip hashes $ take 10 $ filter (>= difficulty) difficulties

    pure ()


powNode' :: NodeConfig TstRealPoWNode -> L.NodeDefinitionL ()
powNode' cfg = do
    L.nodeTag "Tst Real PoW node"

    nodeData <- TstRealPoWNodeData <$> L.newVarIO D.NodeActing

    let workersCount = 4
    let difficulty = 2
    let range = (maxBound :: D.Nonce) `div` workersCount
    let ranges = [(i * range, (i + 1) * range) | i <- [0..workersCount - 1]]

    mapM_ (generateHash nodeData difficulty) ranges

    L.awaitNodeFinished nodeData

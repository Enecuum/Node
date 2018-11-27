{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.GenConfigs where

import qualified Enecuum.Assets.Scenarios           as A
import qualified Enecuum.Assets.TstScenarios        as Tst
import qualified Enecuum.Config                     as Cfg
import qualified Enecuum.Domain                     as D
import           Enecuum.Prelude
import qualified Data.ByteString.Lazy               as B
import           Data.Aeson.Encode.Pretty           (encodePretty)

genConfigs = forM_ configs (uncurry B.writeFile)

configs =
    [ ("configs/poa.json",                  encodePretty $ D.defConfig A.Good       A.defaultPoANodeConfig)
    , ("configs/PoANode_Bad.json",          encodePretty $ D.defConfig A.Bad        A.defaultPoANodeConfig)
    , ("configs/pow.json",                  encodePretty $ D.defConfig A.PoW        A.defaultPoWNodeConfig)
    , ("configs/GN_0.json",                 encodePretty $ D.defConfig A.GN         A.defaultGraphNodeConfig)
    , ("configs/GraphNodeReceiver.json",    encodePretty $ D.defConfig Tst.TstGN    Tst.graphNodeReceiverConfig)
    , ("configs/GraphNodeTransmitter.json", encodePretty $ D.defConfig Tst.TstGN    Tst.graphNodeTransmitterConfig)
    ]

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.GenConfigs where

import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as B
import qualified Enecuum.Assets.Scenarios    as Prd
import qualified Enecuum.Assets.TstScenarios as Tst
import qualified Enecuum.Config              as Cfg
import qualified Enecuum.Domain              as D
import           Enecuum.Prelude

genConfigs = forM_ configs (uncurry B.writeFile)

configs =
    [ ("configs/default/routing_gen_poa.json",            encodePretty $ D.defConfig Prd.Good       Prd.routingGenPoANodeConfig)
    , ("configs/default/routing_gen_poa_bad.json",        encodePretty $ D.defConfig Prd.Bad        Prd.routingGenPoANodeConfig)
    , ("configs/default/routing_gen_pow.json",            encodePretty $ D.defConfig Prd.PoW        Prd.routingGenPoWNodeConfig)
    , ("configs/default/routing_graph_node_0.json",       encodePretty $ D.defConfig Prd.GN         Prd.routingGraphNodeConfig)
    , ("configs/default/routing_multinode.json",          encodePretty $ D.defConfig Prd.MultiNodeS Prd.routingMultiNodeConfig)

    , ("configs/default/tst_graph_node_receiver.json",    encodePretty $ D.defConfig Tst.GN         Tst.tstGraphNodeReceiverConfig)
    , ("configs/default/tst_graph_node_transmitter.json", encodePretty $ D.defConfig Tst.GN         Tst.tstGraphNodeTransmitterConfig)
    ]

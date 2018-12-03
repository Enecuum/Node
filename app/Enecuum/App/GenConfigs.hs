{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType   #-}

module App.GenConfigs where

import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as B
import qualified Enecuum.Assets.TstScenarios as Tst
import qualified Enecuum.Config              as Cfg
import qualified Enecuum.Domain              as D
import           Enecuum.Prelude

genConfigs = forM_ configs (uncurry B.writeFile)

configs =
    [ ("configs/default/tst_graph_node_receiver.json",    encodePretty $ D.defConfig Tst.GN         Tst.tstGraphNodeReceiverConfig)
    , ("configs/default/tst_graph_node_transmitter.json", encodePretty $ D.defConfig Tst.GN         Tst.tstGraphNodeTransmitterConfig)
    ]

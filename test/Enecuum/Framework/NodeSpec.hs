{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Enecuum.Framework.NodeSpec where

import           Test.Hspec
import           Data.Text                                ( Text )
import           Control.Concurrent                       ( threadDelay )
import           Eff                                      ( Eff
                                                          , Member
                                                          , handleRelay
                                                          , handleRelayS
                                                          )
import           Eff.Exc                                  ( Exc
                                                          , throwError
                                                          )
import qualified Eff.Exc.Pure                  as Exc
                                                          ( onFail )
import           Eff.TH                                   ( makeFreer )
import           Eff.SafeIO                               ( SIO
                                                          , safeIO
                                                          , runSafeIO
                                                          )
import           Eff.Reader.Pure                          ( Reader
                                                          , runReader
                                                          )
import qualified Eff.Internal                  as EI
import           Control.Exception                        ( SomeException
                                                          , try
                                                          )
import qualified Data.Aeson                    as A
import           Data.Aeson                               ( ToJSON
                                                          , FromJSON
                                                          )
import qualified Data.ByteString.Lazy          as BS
import           GHC.Generics                             ( Generic )
import           Control.Newtype.Generics                 ( Newtype
                                                          , O
                                                          , pack
                                                          )

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L
import qualified Enecuum.Core.Language         as CL
import qualified Enecuum.Core.Types            as CT

bootNodeTag = "boot_node"
masterNodeTag = "master_node"

-- Some network model is needed.
localNetwork = undefined
externalNetwork = undefined
enecuumNetwork = undefined


withConnection = undefined

request node req = undefined
request' req node = flip request

-- Node description lang
node _ = undefined

-- server :: a -> SomeLang ServerHandle
server _ = undefined

-- startServer _ = undefined


firstOf _ = undefined


waitOne
  :: forall req resp result effs
   . CT.NetworkMethod () req resp
  => Newtype resp
  => Float
  -> (CT.NetworkRequest -> Eff effs ())
  -> req
  -> Eff effs (Maybe (O resp))
waitOne _ networkMethod req = error "waitOne not implemented."


-- -- Discovery as it's described in the document:
-- -- https://docs.enecuum.com/pages/viewpage.action?pageId=3801661
-- bootNodeDiscovery :: (Member CL.NetworkModelL effs) => Eff effs D.NodeConfig
-- bootNodeDiscovery = do
--   mbBootNodeCfg <- firstOf
--     [ waitOne 10.0 (CL.multicast localNetwork)
--       $ D.FindNodeByTagRequest bootNodeTag
--     , waitOne 10.0 (CL.multicast externalNetwork)
--       $ D.FindNodeByTagRequest bootNodeTag
--     , CL.findDNSRecord enecuumNetwork $ D.FindNodeByTagRequest bootNodeTag
--     ]
--   case mbBootNodeCfg of
--     Nothing          -> error "Boot node discovery failed."
--     Just bootNodeCfg -> pure bootNodeCfg





simpleBootNodeDiscovery :: Eff CL.NetworkModelL D.NodeConfig
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- waitSingleResponse 10.0 (CL.multicast localNetwork) $ D.FindNodeByTagRequest bootNodeTag
  case mbBootNodeCfg of
    Nothing          -> error "Boot node discovery failed."
    Just bootNodeCfg -> pure bootNodeCfg

findNodeWith
  :: CL.NodeLanguage effs
  => Eff effs D.NodeConfig
findNodeWith discoveryAlg = undefined


data NodeEndpoint = NodeEndpoint
  { nodeID :: D.NodeID
  , serverHandle :: D.ServerHandle
  }

acceptHello1
  :: HelloRequest1 -> Eff L.NodeInteractionL ()
acceptHello1 (HelloRequest1 msg) = error $ "Accepting HelloRequest1: " ++ msg

acceptHello2
  :: HelloRequest2 -> Eff L.NodeInteractionL ()
acceptHello2 (HelloRequest2 msg) = error $ "Accepting HelloRequest2: " ++ msg

data NodeDef = NodeDef
    { nodeTag :: D.NodeTag
    , nodeScenario :: Eff L.NodeDefinitionL NodeEndpoint
    }

data GetNeighboursRequest = GetNeighboursRequest
  deriving (Generic, ToJSON, FromJSON)
data GetHashIDRequest = GetHashIDRequest
  deriving (Generic, ToJSON, FromJSON)
data AcceptConnectionRequest = AcceptConnectionRequest
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest1 = HelloRequest1 String
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest2 = HelloRequest2 String
  deriving (Generic, ToJSON, FromJSON)


-- bootNode = node bootNodeTag $ do
--   nodeID <- initialization $ do
--     pure $ NodeID hashID
--   serverHandle <- serving $ serveRequest sendHashID
--   pure $ Node nodeID serverHandle
--  where
--    sendHashID _ = undefined

masterNodeServerDef :: L.HandlersF
masterNodeServerDef
  = L.serveRequest @HelloRequest1 acceptHello1
  . L.serveRequest @HelloRequest2 acceptHello2

masterNodeInitialization :: Eff L.NodeInteractionL D.NodeID
masterNodeInitialization = do
  bootNodeCfg <- findNodeWith simpleBootNodeDiscovery
  hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
  pure $ D.NodeID hashID

masterNodeScenario
  :: forall effs
   . D.Config
  -> Eff L.NodeDefinitionL NodeEndpoint
masterNodeScenario bootNodeCfg = do

  nodeID       <- L.initialization masterNodeInitialization
  serverHandle <- L.serving masterNodeServerDef

  pure $ NodeEndpoint nodeID serverHandle

masterNodeDef :: D.Config -> NodeDef
masterNodeDef cfg = NodeDef masterNodeTag (masterNodeScenario cfg)


runNode = undefined

spec :: Spec
spec = describe "Master Node test" $ it "Master Node test" $ do

  res <- runNode $ masterNodeDef $ D.Config "boot node addr"

  threadDelay 1000

  "a" `shouldBe` ("b" :: String)

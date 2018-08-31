{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
                                                          , unpack
                                                          )

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L

bootNodeTag = "bootNode"
masterNodeTag = "masterNode"

localNetwork = undefined
externalNetwork = undefined
enecuumNetwork = undefined

withConnection = undefined

request node req = undefined
request' req node = flip request

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

data NodeEndpoint = NodeEndpoint
  { nodeID :: D.NodeID
  , serverHandle :: D.ServerHandle
  }

simpleBootNodeDiscovery :: Eff L.NetworkModel D.NodeConfig
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- fmap unpack <$> (L.multicastRequest localNetwork 10.0 $ D.FindNodeByTagRequest bootNodeTag)
  case mbBootNodeCfg of
    Nothing          -> error "Boot node discovery failed."
    Just bootNodeCfg -> pure bootNodeCfg


acceptHello1 :: HelloRequest1 -> Eff L.NodeModel ()
acceptHello1 (HelloRequest1 msg) = error $ "Accepting HelloRequest1: " ++ msg

acceptHello2 :: HelloRequest2 -> Eff L.NodeModel ()
acceptHello2 (HelloRequest2 msg) = error $ "Accepting HelloRequest2: " ++ msg


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

bootNode :: Eff L.NodeDefinitionModel NodeEndpoint
bootNode = do
  L.nodeTag bootNodeTag
  nodeID <- L.initialization $ pure $ D.NodeID "abc"
  serverHandle <- L.serving $ L.serveRequest @HelloRequest1 acceptHello1
  pure $ NodeEndpoint nodeID serverHandle

masterNodeServerDef :: L.HandlersF
masterNodeServerDef
  = L.serveRequest @HelloRequest1 acceptHello1
  . L.serveRequest @HelloRequest2 acceptHello2

masterNodeInitialization :: Eff L.NodeModel D.NodeID
masterNodeInitialization = do
  bootNodeCfg <- L.evalNetwork simpleBootNodeDiscovery
  hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
  pure $ D.NodeID hashID

masterNode :: D.Config -> Eff L.NodeDefinitionModel NodeEndpoint
masterNode bootNodeCfg = do
  L.nodeTag masterNodeTag
  nodeID       <- L.initialization masterNodeInitialization
  serverHandle <- L.serving masterNodeServerDef
  pure $ NodeEndpoint nodeID serverHandle

runNode = undefined

spec :: Spec
spec = describe "Master Node test" $ it "Master Node test" $ do

  res <- runNode $ masterNode $ D.Config "boot node addr"

  threadDelay 1000

  "a" `shouldBe` ("b" :: String)

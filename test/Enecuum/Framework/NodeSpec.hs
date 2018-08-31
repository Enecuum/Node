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

findNodeWith discoveryAlg = undefined

bootNodeTag = "boot_node"
masterNodeTag = "master_node"

-- Some network model is needed.
localNetwork = undefined
externalNetwork = undefined
enecuumNetwork = undefined


connect = undefined
withConnection = undefined

request node req = undefined
request' req node = flip request


data ServerHandle = ServerHandle
  {

  }

-- Node description lang
node _ = undefined
initialization _ = undefined

-- server :: a -> SomeLang ServerHandle
server _ = undefined

-- startServer _ = undefined

newtype NodeID = NodeID Text



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

-- Simple discovery
simpleBootNodeDiscovery
  :: (Member CL.NetworkModelL effs) => Eff effs D.NodeConfig
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- waitOne 10.0 (CL.multicast localNetwork)
    $ D.FindNodeByTagRequest bootNodeTag
  case mbBootNodeCfg of
    Nothing          -> error "Boot node discovery failed."
    Just bootNodeCfg -> pure bootNodeCfg


data Node = Node
  { nodeID :: NodeID
  , serverHandle :: ServerHandle
  }

acceptConnection _ = undefined
acceptHello _ = undefined

data ServingL a where
  Dummy :: ServingL a

data GetNeighboursRequest = GetNeighboursRequest
  deriving (Generic, ToJSON, FromJSON)
data GetHashIDRequest = GetHashIDRequest
  deriving (Generic, ToJSON, FromJSON)
data AcceptConnectionRequest = AcceptConnectionRequest
  deriving (Generic, ToJSON, FromJSON)
data HelloRequest = HelloRequest
  deriving (Generic, ToJSON, FromJSON)

sendHashID :: (Member ServingL effs) => GetHashIDRequest -> Eff effs ()
sendHashID _ = undefined

serveRequest
  :: (FromJSON req)
  => (req -> Eff effs ())
  -> (Eff effs (), Maybe BS.ByteString)
  -> (Eff effs (), Maybe BS.ByteString)
serveRequest handler (handled, Just rawReq) = case A.decode rawReq of
  Just req -> (handled >> handler req, Nothing)
  Nothing  -> (handled, Just rawReq)
serveRequest handler (handled, Nothing) = (handled, Nothing)



serving handlersF = undefined




-- bootNode = node bootNodeTag $ do
--   nodeID <- initialization $ do
--     bootNodeCfg <- findNodeWith simpleBootNodeDiscovery
--     hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
--     pure $ NodeID hashID

--   serverHandle <- serving $ serveRequest sendHashID

--   pure $ Node nodeID serverHandle



masterNodeScenario
  :: forall effs . (Member L.NodeL effs) => D.Config -> Eff effs Node
masterNodeScenario bootNodeCfg = do

  -- nodeID <- initialization $ do
  --   bootNodeCfg <- findNodeWith simpleBootNodeDiscovery
  --   hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
  --   pure $ NodeID hashID

  serverHandle <- serving $ serveRequest @HelloRequest acceptHello

  pure $ Node (NodeID "") serverHandle



data NodeDef = NodeDef
  { nodeTag :: D.NodeTag
  , nodeScenario :: forall effs. (Member L.NodeL effs) => Eff effs Node
  }

masterNode :: D.Config -> NodeDef
masterNode cfg = NodeDef masterNodeTag (masterNodeScenario cfg)


runNode = undefined

spec :: Spec
spec = describe "Master Node test" $ it "Master Node test" $ do

  res <- runNode $ masterNode $ D.Config "boot node addr"

  threadDelay 1000

  "a" `shouldBe` "b"

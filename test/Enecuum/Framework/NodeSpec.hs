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
import GHC.Generics (Generic)

import qualified Enecuum.Domain                as D
import qualified Enecuum.Language              as L

type NodeRequestResult a = Either String a

data Request = Request
data Response = Response

-- class NodeRequest cfg req resp | req -> resp, resp -> req where
--     toRequest :: cfg -> Request
--     fromResponse :: cfg -> Response -> NodeRequestResult b


data NodeFilter = ByTag Text

data MulticastRequest = FindNode NodeFilter

findNodeByTag = FindNode . ByTag


findNodeWith discoveryAlg = undefined

bootNodeTag = "boot_node"
masterNodeTag = "master_node"

-- Some network model is needed.
localNetwork = undefined
externalNetwork = undefined
enecuumNetwork = undefined

firstOf _ = undefined

-- Node discovery lang
multicast = undefined
waitOne _ = undefined


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


-- Discovery as it described in the document:
-- https://docs.enecuum.com/pages/viewpage.action?pageId=3801661
bootNodeDiscovery = do
  mbBootNodeCfg <- firstOf
    [ waitOne 10.0 $ multicast localNetwork $ findNodeByTag bootNodeTag
    , waitOne 10.0 $ multicast externalNetwork $ findNodeByTag bootNodeTag
    , waitOne 10.0 $ multicast enecuumNetwork $ findNodeByTag bootNodeTag
    ]
  case mbBootNodeCfg of
    Nothing          -> error "Boot node discovery failed."
    Just bootNodeCfg -> pure bootNodeCfg

-- Simple discovery
simpleBootNodeDiscovery = do
  mbBootNodeCfg <- waitOne 10.0 $ multicast localNetwork $ findNodeByTag
    bootNodeTag
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



data ReqEff req a


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



masterNodeScenario :: (Member L.NodeL effs) => D.Config -> Eff effs Node
masterNodeScenario bootNodeCfg = do

  -- nodeID <- initialization $ do
  --   bootNodeCfg <- findNodeWith simpleBootNodeDiscovery
  --   hashID      <- withConnection bootNodeCfg $ request' GetHashIDRequest
  --   pure $ NodeID hashID

  serverHandle <- serving $ serveRequest @HelloRequest acceptHello

  pure $ Node (NodeID "") serverHandle



data NodeDef = NodeDef
  { nodeTag :: D.NodeTag
  , nodeScenario :: forall effs. (Member L.NodeL effs) => Eff effs ()
  }

masterNode :: D.Config -> NodeDef
masterNode = NodeDef masterNodeTag . masterNodeScenario


runNode = undefined

spec :: Spec
spec = describe "Master Node test" $ it "Master Node test" $ do

  res <- runNode $ masterNode $ D.Config "boot node addr"

  threadDelay 1000

  "a" `shouldBe` "b"



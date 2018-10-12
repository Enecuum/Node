{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Enecuum.Framework.NodeDefinition.Language where

import           Enecuum.Prelude


import qualified Enecuum.Core.Language                 as L
import qualified Enecuum.Framework.Domain              as D
import qualified Enecuum.Framework.Node.Language       as L
import qualified Enecuum.Framework.Networking.Language as L
import           Enecuum.Legacy.Service.Network.Base
import           Enecuum.Framework.Handler.Rpc.Language  (RpcHandlerL)
import           Enecuum.Framework.Handler.Tcp.Language
import           Enecuum.Framework.Handler.Udp.Language
import           Enecuum.Framework.Handler.Cmd.Language
import           Language.Haskell.TH.MakeFunctor (makeFunctorInstance)

-- TODO: it's possible to make these steps evaluating step-by-step, in order.
-- Think about if this really needed.

-- | Node description language.
-- Allows to specify what actions should be done when node starts.
data NodeDefinitionF next where
    -- | Set node tag. For example, "boot node".
    NodeTag        :: D.NodeTag -> (() -> next) -> NodeDefinitionF next
    -- | Evaluate some node model.
    EvalNodeL      :: L.NodeL  a -> (a -> next) -> NodeDefinitionF next
    -- | Eval core effect.
    EvalCoreEffectNodeDefinitionF :: L.CoreEffect a -> (a -> next) -> NodeDefinitionF next
    -- | Start serving of RPC requests.
    ServingRpc     :: PortNumber -> RpcHandlerL L.NodeL () -> (() -> next) -> NodeDefinitionF next
    -- | Stop serving of Rpc server.
    StopServing    :: PortNumber -> (() -> next) -> NodeDefinitionF next
    -- | Start serving on reliable-kind connection.
    ServingTcp     :: PortNumber -> TcpHandlerL L.NodeL () -> (() -> next)-> NodeDefinitionF  next
    ServingUdp     :: PortNumber -> UdpHandlerL L.NodeL () -> (() -> next)-> NodeDefinitionF  next
    Std            :: CmdHandlerL () -> (() -> next) -> NodeDefinitionF  next
    -- Process interface. TODO: It's probably wise to move it to own language.
    -- | Fork a process for node.
    ForkProcess :: L.NodeL a -> (D.ProcessPtr a -> next) -> NodeDefinitionF next
    -- | Try get result (non-blocking).
    TryGetResult :: D.ProcessPtr a -> (Maybe a -> next) -> NodeDefinitionF next
    -- | Await for result (blocking).
    AwaitResult :: D.ProcessPtr a -> (a -> next) -> NodeDefinitionF next


makeFunctorInstance ''NodeDefinitionF

type NodeDefinitionL next = Free NodeDefinitionF next

std :: CmdHandlerL () -> NodeDefinitionL ()
std handlers = liftF $ Std handlers id

-- | Sets tag for node.
nodeTag :: D.NodeTag -> NodeDefinitionL ()
nodeTag tag = liftF $ NodeTag tag id

-- | Runs node scenario.
evalNodeL :: L.NodeL a -> NodeDefinitionL a
evalNodeL action = liftF $ EvalNodeL action id

-- | Fork a process for node.
fork :: L.NodeL a -> NodeDefinitionL (D.ProcessPtr a)
fork action = liftF $ ForkProcess action id

-- | Fork a process for node.
process :: L.NodeL () -> NodeDefinitionL ()
process = void . fork

-- | Try get result from a process (non-blocking).
tryGetResult :: D.ProcessPtr a -> NodeDefinitionL (Maybe a)
tryGetResult handle = liftF $ TryGetResult handle id

-- | Await for result from a process (blocking).
awaitResult :: D.ProcessPtr a -> NodeDefinitionL a
awaitResult handle = liftF $ AwaitResult handle id

-- | Eval core effect.
evalCoreEffectNodeDefinitionF :: L.CoreEffect a -> NodeDefinitionL a
evalCoreEffectNodeDefinitionF coreEffect = liftF $ EvalCoreEffectNodeDefinitionF coreEffect id

-- | Runs scenario as initialization.
initialization :: L.NodeL a -> NodeDefinitionL a
initialization = evalNodeL

-- | Runs scenario.
scenario :: L.NodeL a -> NodeDefinitionL a
scenario = evalNodeL

class Serving a where
    serving :: PortNumber -> a -> NodeDefinitionL ()

instance Serving (RpcHandlerL L.NodeL ()) where
    serving = servingRpc

instance Serving (TcpHandlerL L.NodeL ()) where
    serving port handlersF = liftF $ ServingTcp port handlersF id

instance Serving (UdpHandlerL L.NodeL ()) where
    serving port handlersF = liftF $ ServingUdp port handlersF id

instance L.Connection (Free NodeDefinitionF) D.TcpConnection (TcpHandlerL L.NodeL ()) where
    close conn       = evalNodeL $ L.close conn
    open  addr handl = evalNodeL $ L.open  addr handl

instance L.Connection (Free NodeDefinitionF) D.UdpConnection (UdpHandlerL L.NodeL ()) where
    close conn       = evalNodeL $ L.close conn
    open  addr handl = evalNodeL $ L.open  addr handl

instance L.Send a L.NodeL => L.Send a (Free NodeDefinitionF) where
    send conn msg = evalNodeL $ L.send conn msg

-- | Starts RPC server.
{-# DEPRECATED servingRpc "Use L.serving" #-}
servingRpc :: PortNumber -> RpcHandlerL L.NodeL () -> NodeDefinitionL ()
servingRpc port handlersF = liftF $ ServingRpc port handlersF id

-- | Stops server on the specified port.
-- TODO: What is the behavior when server is absent?
stopServing :: PortNumber -> NodeDefinitionL ()
stopServing port = liftF $ StopServing port id

-- | Starts server (TCP / WS - like)
servingMsg :: PortNumber -> TcpHandlerL L.NodeL () -> NodeDefinitionL ()
servingMsg port handlersF = liftF $ ServingTcp port handlersF id


instance L.Logger (Free NodeDefinitionF) where
    logMessage level msg = evalCoreEffectNodeDefinitionF $ L.logMessage level msg

instance L.ERandom (Free NodeDefinitionF) where
    getRandomInt =  evalCoreEffectNodeDefinitionF . L.getRandomInt
    evalRand r g = evalCoreEffectNodeDefinitionF  $ L.evalRand r g
    generateKeyPair = evalCoreEffectNodeDefinitionF $ L.generateKeyPair
    sign key msg = evalCoreEffectNodeDefinitionF $ L.sign key msg

instance L.ControlFlow (Free NodeDefinitionF) where
    delay = evalCoreEffectNodeDefinitionF . L.delay

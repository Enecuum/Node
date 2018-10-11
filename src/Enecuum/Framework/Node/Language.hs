{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}


module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain.Networking      as D
import           Enecuum.Framework.Handler.Tcp.Language
import           Enecuum.Framework.Handler.Udp.Language
import qualified Enecuum.Core.Types                       as T
import           Language.Haskell.TH.MakeFunctor

-- | Node language.
data NodeF next where
    -- | Eval stateful action atomically.
    EvalStateAtomically :: L.StateL a -> (a -> next) -> NodeF next
    -- | Eval networking.
    EvalNetworking :: L.NetworkingL a -> (a -> next) -> NodeF next
    -- | Eval core effect.
    EvalCoreEffectNodeF :: L.CoreEffect a -> (a -> next) -> NodeF next
    -- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
    EvalGraphIO :: (Serialize c, T.StringHashable c) => T.TGraph c -> Free (L.HGraphF (T.TNodeL c)) x -> (x -> next) -> NodeF next
    NewGraph  :: (Serialize c, T.StringHashable c) => (T.TGraph c -> next) -> NodeF next
    -- | Stop the node evaluation
    StopNode :: (() -> next) -> NodeF next
    -- | Open connection to the node.
    OpenTcpConnection :: D.Address -> TcpHandlerL NodeL () -> (D.TcpConnection -> next) -> NodeF next
    OpenUdpConnection :: D.Address -> UdpHandlerL NodeL () -> (D.UdpConnection -> next) -> NodeF next
    -- | Close existing connection.
    CloseTcpConnection :: D.TcpConnection -> (() -> next) -> NodeF  next
    CloseUdpConnection :: D.UdpConnection -> (() -> next) -> NodeF  next

type NodeL = Free NodeF

makeFunctorInstance ''NodeF

-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> NodeL a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- | Alias for convenience.
atomically :: L.StateL a -> NodeL a
atomically = evalStateAtomically

-- TODO: makeLanguage ''NodeF
-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeL a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffect a -> NodeL a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

-- | Stop of node eval.
stopNode :: NodeL ()
stopNode = liftF $ StopNode id

-- | Open network connection.
{-# DEPRECATED openConnection "Use L.open" #-}
openConnection :: D.Address -> TcpHandlerL NodeL () -> NodeL D.TcpConnection
openConnection = open

-- | Close network connection.
-- TODO: what is the behavior when connection is closed?
{-# DEPRECATED closeConnection "Use L.close" #-}
closeConnection :: D.TcpConnection -> NodeL ()
closeConnection = close

class Connection a con hend | con -> hend where
    close :: con -> a ()
    open  :: D.Address -> hend -> a con

instance Connection (Free NodeF) D.TcpConnection (TcpHandlerL NodeL ()) where
    close conn       = liftF $ CloseTcpConnection conn id
    open  addr handl = liftF $ OpenTcpConnection  addr handl id

instance Connection (Free NodeF) D.UdpConnection (UdpHandlerL NodeL ()) where
    close conn       = liftF $ CloseUdpConnection conn id
    open  addr handl = liftF $ OpenUdpConnection  addr handl id

instance L.Send a (Free L.NetworkingF) => L.Send a NodeL where
    send conn msg = evalNetworking $ L.send conn msg

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO :: (T.StringHashable c, Serialize c) => T.TGraph c -> Free (L.HGraphF (T.TNodeL c)) a -> NodeL a
evalGraphIO g graphAction = liftF $ EvalGraphIO g graphAction id

instance L.Logger (Free NodeF) where
    logMessage level msg = evalCoreEffectNodeF $ L.logMessage level msg

instance L.ERandom (Free NodeF) where
    getRandomInt = evalCoreEffectNodeF . L.getRandomInt
    evalRand r g = evalCoreEffectNodeF $ L.evalRand r g

instance L.ControlFlow (Free NodeF) where
    delay =  evalCoreEffectNodeF . L.delay

newGraph :: (Serialize c, T.StringHashable c) => NodeL (T.TGraph c)
newGraph = liftF $ NewGraph id

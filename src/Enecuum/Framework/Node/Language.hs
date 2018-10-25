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
import qualified Enecuum.Core.Types                       as T
import           Enecuum.Framework.Handler.Network.Language
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
    -- | Create new graph instance.
    NewGraph  :: (Serialize c, T.StringHashable c) => (T.TGraph c -> next) -> NodeF next
    -- | Open connection to the node.
    OpenTcpConnection :: D.Address -> NetworkHandlerL D.Tcp NodeL () -> (D.Connection D.Tcp -> next) -> NodeF next
    OpenUdpConnection :: D.Address -> NetworkHandlerL D.Udp NodeL () -> (D.Connection D.Udp -> next) -> NodeF next
    -- | Close existing connection.
    CloseTcpConnection :: D.Connection D.Tcp -> (() -> next) -> NodeF  next
    CloseUdpConnection :: D.Connection D.Udp -> (() -> next) -> NodeF  next

type NodeL = Free NodeF

makeFunctorInstance ''NodeF

-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> NodeL a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- TODO: makeLanguage ''NodeF
-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeL a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffect a -> NodeL a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

withConnection
    :: (Monad m, Connection m con)
    => con -> D.Address -> (D.Connection con -> m b) -> m b
withConnection protocol address f = do
    con <- open protocol address $ pure ()
    a <- f con
    close con
    pure a

class Connection a con where
    close :: D.Connection con -> a ()
    open  :: con -> D.Address -> NetworkHandlerL con NodeL () -> a (D.Connection con)

instance Connection (Free NodeF) D.Tcp where
    close   conn       = liftF $ CloseTcpConnection conn id
    open  _ addr handl = liftF $ OpenTcpConnection  addr handl id

instance Connection (Free NodeF) D.Udp where
    close   conn       = liftF $ CloseUdpConnection conn id
    open  _ addr handl = liftF $ OpenUdpConnection  addr handl id

instance L.Send a (Free L.NetworkingF) => L.Send a NodeL where
    send conn msg = evalNetworking $ L.send conn msg

instance L.SendUdp NodeL where
    notify conn msg = evalNetworking $ L.notify conn msg

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO :: (T.StringHashable c, Serialize c) => T.TGraph c -> Free (L.HGraphF (T.TNodeL c)) a -> NodeL a
evalGraphIO g graphAction = liftF $ EvalGraphIO g graphAction id

newGraph :: (Serialize c, T.StringHashable c) => NodeL (T.TGraph c)
newGraph = liftF $ NewGraph id

instance L.Logger (Free NodeF) where
    logMessage level msg = evalCoreEffectNodeF $ L.logMessage level msg

instance L.ERandom (Free NodeF) where
    evalCoreCrypto = evalCoreEffectNodeF . L.evalCoreCrypto
    getRandomInt = evalCoreEffectNodeF . L.getRandomInt
    getRandomByteString = evalCoreEffectNodeF . L.getRandomByteString
    nextUUID = evalCoreEffectNodeF $ L.nextUUID

instance L.FileSystem (Free NodeF) where
    readFile = evalCoreEffectNodeF . L.readFile
    getHomeDirectory = evalCoreEffectNodeF L.getHomeDirectory
    createFilePath filepath = evalCoreEffectNodeF $ L.createFilePath filepath 

instance L.ControlFlow (Free NodeF) where
    delay =  evalCoreEffectNodeF . L.delay

instance L.StateIO (Free NodeF) where
    atomically = evalStateAtomically
    newVarIO  = evalStateAtomically . L.newVar
    readVarIO = evalStateAtomically . L.readVar
    writeVarIO var a = evalStateAtomically $ L.writeVar var a

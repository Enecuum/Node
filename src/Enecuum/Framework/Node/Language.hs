{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE BangPatterns           #-}


module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain.Networking      as D
import qualified Enecuum.Core.Types                       as D
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
    EvalGraphIO :: (Serialize c, D.StringHashable c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) x -> (x -> next) -> NodeF next
    -- | Create new graph instance.
    NewGraph  :: (Serialize c, D.StringHashable c) => (D.TGraph c -> next) -> NodeF next
    -- | Open connection to the node.
    OpenTcpConnection :: D.Address -> NetworkHandlerL D.Tcp NodeL () -> (Maybe (D.Connection D.Tcp) -> next) -> NodeF next
    OpenUdpConnection :: D.Address -> NetworkHandlerL D.Udp NodeL () -> (Maybe (D.Connection D.Udp) -> next) -> NodeF next
    -- | Close existing connection.
    CloseTcpConnection :: D.Connection D.Tcp -> (() -> next) -> NodeF  next
    CloseUdpConnection :: D.Connection D.Udp -> (() -> next) -> NodeF  next
    -- | Create database with config.
    InitDatabase :: D.DBConfig db -> (D.DBResult (D.Storage db) -> next) -> NodeF next
    -- | Eval database.
    EvalDatabase :: D.Storage db -> L.DatabaseL db a -> (a -> next) -> NodeF next

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

-- | Init database with the options passed for the specific storage type.
initDatabase :: D.DBConfig db -> NodeL (D.DBResult (D.Storage db))
initDatabase config = liftF $ InitDatabase config id

-- | Eval database.
evalDatabase :: D.Storage db -> L.DatabaseL db a -> NodeL a
evalDatabase db script = liftF $ EvalDatabase db script id

-- | Eval database.
withDatabase :: D.Storage db -> L.DatabaseL db a -> NodeL a
withDatabase = evalDatabase

withConnection
    :: (Monad m, Connection m con)
    => con -> D.Address -> (D.Connection con -> m b) -> m (Maybe b)
withConnection protocol address f = do
    mCon <- open protocol address $ pure ()
    case mCon of
        Just con -> do 
            !a <- f con
            close con
            pure $ Just a
        Nothing -> pure Nothing


listener !f = handler (\a conn -> void (close conn) >> f a)

class Connection a con where
    close :: D.Connection con -> a ()
    open  :: con -> D.Address -> NetworkHandlerL con NodeL () -> a (Maybe (D.Connection con))

instance Connection (Free NodeF) D.Tcp where
    close   conn       = liftF $ CloseTcpConnection conn id
    open  _ addr handl = liftF $ OpenTcpConnection  addr handl id

instance Connection (Free NodeF) D.Udp where
    close   conn       = liftF $ CloseUdpConnection conn id
    open  _ addr handl = liftF $ OpenUdpConnection  addr handl id

instance L.Send a (Free L.NetworkingF) => L.Send a NodeL where
    send conn   = evalNetworking . L.send conn

instance L.SendUdp NodeL where
    notify conn = evalNetworking . L.notify conn

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO :: (D.StringHashable c, Serialize c) => D.TGraph c -> Free (L.HGraphF (D.TNodeL c)) a -> NodeL a
evalGraphIO g graphAction = liftF $ EvalGraphIO g graphAction id

newGraph :: (Serialize c, D.StringHashable c) => NodeL (D.TGraph c)
newGraph = liftF $ NewGraph id

instance L.IOL NodeL where
    evalIO = evalCoreEffectNodeF . L.evalIO 

instance L.Logger NodeL where
    logMessage level = evalCoreEffectNodeF . L.logMessage level

instance L.ERandom NodeL where
    evalCoreCrypto      = evalCoreEffectNodeF . L.evalCoreCrypto
    getRandomInt        = evalCoreEffectNodeF . L.getRandomInt
    getRandomByteString = evalCoreEffectNodeF . L.getRandomByteString
    nextUUID            = evalCoreEffectNodeF   L.nextUUID

instance L.FileSystem NodeL where
    readFile         = evalCoreEffectNodeF . L.readFile
    getHomeDirectory = evalCoreEffectNodeF   L.getHomeDirectory
    createFilePath   = evalCoreEffectNodeF . L.createFilePath 

instance L.ControlFlow NodeL where
    delay = evalCoreEffectNodeF . L.delay

instance L.StateIO NodeL where
    atomically     = evalStateAtomically
    newVarIO       = evalStateAtomically . L.newVar
    readVarIO      = evalStateAtomically . L.readVar
    writeVarIO var = evalStateAtomically . L.writeVar var

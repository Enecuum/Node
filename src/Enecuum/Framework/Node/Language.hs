{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain.Networking      as D
import           Enecuum.Framework.MsgHandler.Language
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
  OpenConnection :: D.Address -> MsgHandlerL NodeL () -> (D.NetworkConnection -> next) -> NodeF next
  -- | Close existing connection.
  CloseConnection :: D.NetworkConnection -> (() -> next) -> NodeF  next

type NodeL = Free NodeF

makeFunctorInstance ''NodeF



-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> NodeL  a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- | Alias for convenience.
atomically :: L.StateL a -> NodeL a
atomically = evalStateAtomically


-- TODO: makeLanguage ''NodeF
-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeL  a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffect a -> NodeL a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

-- | Stop of node eval.
stopNode :: NodeL ()
stopNode = liftF $ StopNode id

-- | Open network connection.
{-# DEPRECATED openConnection "Use L.open" #-}
openConnection :: D.Address -> MsgHandlerL NodeL () -> NodeL D.NetworkConnection
openConnection = open

-- | Close network connection.
-- TODO: what is the behavior when connection is closed?
{-# DEPRECATED closeConnection "Use L.close" #-}
closeConnection :: D.NetworkConnection -> NodeL ()
closeConnection = close



class Connection a where
  close :: D.NetworkConnection -> a ()
  open  :: D.Address -> MsgHandlerL NodeL () -> a D.NetworkConnection

instance Connection (Free NodeF) where
  close conn = liftF $ CloseConnection conn id
  open addr handl = liftF $ OpenConnection addr handl id

instance L.Send NodeL where
  send conn msg = evalNetworking $ L.send conn msg

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO
  :: ( T.StringHashable c
     , Serialize c
     )
  => T.TGraph c
  -> Free (L.HGraphF (T.TNodeL c)) a
  -> NodeL a
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

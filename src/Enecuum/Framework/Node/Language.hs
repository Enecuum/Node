{-# LANGUAGE GADTs #-}

module Enecuum.Framework.Node.Language where

import           Enecuum.Prelude

import qualified Data.Aeson                               as A

import qualified Enecuum.Core.Types                       as T
import qualified Enecuum.Core.Language                    as L
import qualified Enecuum.Framework.State.Language         as L
import qualified Enecuum.Framework.Networking.Language    as L
import qualified Enecuum.Framework.Domain                 as D

-- | Node language.
data NodeF next where
  -- | Eval stateful action atomically.
  EvalStateAtomically :: L.StateL a -> (a -> next) -> NodeF next
  -- | Eval networking.
  EvalNetworking :: L.NetworkingL a -> (a -> next) -> NodeF next
  -- | Eval core effect.
  EvalCoreEffectNodeF :: L.CoreEffectModel a -> (a -> next) -> NodeF next
  -- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
  EvalGraphIO :: L.GraphModel a -> (a -> next) -> NodeF next

instance Functor NodeF where
  fmap g (EvalStateAtomically statefulAction next) = EvalStateAtomically statefulAction (g . next)
  fmap g (EvalNetworking networking next)          = EvalNetworking networking          (g . next)
  fmap g (EvalCoreEffectNodeF coreEffect next)     = EvalCoreEffectNodeF coreEffect     (g . next)
  fmap g (EvalGraphIO graphAction next)            = EvalGraphIO graphAction            (g . next)

type NodeModel next = Free NodeF next

-- | Eval stateful action atomically.
evalStateAtomically :: L.StateL a -> NodeModel a
evalStateAtomically statefulAction = liftF $ EvalStateAtomically statefulAction id

-- | Alias for convenience.
atomically :: L.StateL a -> NodeModel a
atomically = evalStateAtomically

-- | Eval networking.
evalNetworking :: L.NetworkingL a -> NodeModel a
evalNetworking newtorking = liftF $ EvalNetworking newtorking id

-- | Eval core effect.
evalCoreEffectNodeF :: L.CoreEffectModel a -> NodeModel a
evalCoreEffectNodeF coreEffect = liftF $ EvalCoreEffectNodeF coreEffect id

-- | Eval graph non-atomically (parts of script are evaluated atomically but separated from each other).
evalGraphIO :: L.GraphModel a -> NodeModel a
evalGraphIO graphAction = liftF $ EvalGraphIO graphAction id

instance L.Logger (Free NodeF) where
  logMessage level msg = evalCoreEffectNodeF $ L.logMessage level msg

-- Raw idea of RPC description. Will be reworked.

-- | Handler is a function which processes a particular response
-- if this response is what RawData contains.
type Handler = (NodeModel (Maybe D.RawData), D.RawData)

-- | HandlersF is a function holding stack of handlers which are handling
-- different requests.
type HandlersF = Handler -> Handler

-- | Tries to decode a request into a request the handler accepts.
-- On success, calls the handler and returns Just result.
-- On failure, returns Nothing.
tryHandler
  :: D.RpcMethod () req resp
  => FromJSON req
  => ToJSON resp
  => (req -> NodeModel resp)
  -> D.RawData
  -> NodeModel (Maybe D.RawData)
tryHandler handler rawReq = case A.decode rawReq of
  Nothing -> pure Nothing
  Just req -> do
    resp <- handler req
    pure $ Just $ A.encode resp

-- | Allows to specify a stack of handlers for different RPC requests.
serve
  :: D.RpcMethod () req resp
  => FromJSON req
  => ToJSON resp
  => (req -> NodeModel resp)
  -> (NodeModel (Maybe D.RawData), D.RawData)
  -> (NodeModel (Maybe D.RawData), D.RawData)
serve handler (prevHandled, rawReq) = (newHandled, rawReq)
  where
    newHandled = prevHandled >>= \case
      Nothing      -> tryHandler handler rawReq
      Just rawResp -> pure $ Just rawResp

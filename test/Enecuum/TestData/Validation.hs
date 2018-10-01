{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Enecuum.TestData.Validation where

import Enecuum.Prelude
import Data.Validation
import Control.Lens

import qualified Enecuum.Language as L

data ValidationRequest = ValidRequest | InvalidRequest
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype ValidationResponse = ValidationResponse (Either [Text] Text)
  deriving (Show, Eq, Generic, Newtype, ToJSON, FromJSON)

verifyRequest :: ValidationRequest -> Validation [Text] Text
verifyRequest ValidRequest = _Success # "correct"
verifyRequest InvalidRequest = _Failure # ["invalid"]

makeResponse :: Validation [Text] Text -> ValidationResponse
makeResponse = ValidationResponse . toEither

acceptValidationRequest :: ValidationRequest -> L.NodeL ValidationResponse
acceptValidationRequest req   = pure $ makeResponse $ verifyRequest req
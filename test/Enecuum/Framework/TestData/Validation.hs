{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Enecuum.Framework.TestData.Validation where

import Enecuum.Prelude
import Data.Validation 
import Control.Lens
import Enecuum.Framework.TestData.RPC (ValidationRequest(..), ValidationResponse(..))

verifyRequest :: ValidationRequest -> Validation [Text] Text
verifyRequest ValidRequest = _Success # "correct"
verifyRequest InvalidRequest = _Failure # ["invalid"]

makeResponse :: Validation [Text] Text -> ValidationResponse
makeResponse = ValidationResponse . toEither

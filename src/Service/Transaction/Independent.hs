{-# LANGUAGE DuplicateRecordFields #-}

module Service.Transaction.Independent where
import           Control.Exception
-- import           Control.Monad
import qualified Data.Serialize                   as S (decode, encode)
import           Node.Data.GlobalLoging
import           Service.InfoMsg                  (LogingTag (..), MsgType (..))
import           Service.Transaction.SproutCommon
import           Service.Transaction.Storage
import           Service.Types

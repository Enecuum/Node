module Enecuum.Core.Types.Logger where
import           Prelude


-- | Logging level.
data LogLevel = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

module Enecuum.Legacy.Service.Sync.SyncTypes where
import           Enecuum.Legacy.Service.Types
import           Enecuum.Prelude

data KeyBlockContent = KeyBlockContent KeyBlockInfoPoW [HashOfMicroblock] deriving Show
data MicroBlockContent = MicroBlockContent Microblock deriving Show
type From = Number
type To = Number
type Limit = Integer

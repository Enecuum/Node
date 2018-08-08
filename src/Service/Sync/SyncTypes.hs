module Service.Sync.SyncTypes where
import           Service.Types


-- data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]

data KeyBlockContent = KeyBlockContent KeyBlockInfoPoW [HashOfMicroblock] deriving Show
data MicroBlockContent = MicroBlockContent Microblock deriving Show
type From = Number
type To = Number
type Limit = Integer

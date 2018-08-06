module Service.Sync.SyncTypes where
import           Service.Types


data MicroBlockContent = MicroBlockContent MicroblockBD [TransactionInfo]
type From = Number
type To = Number
type Limit = Integer

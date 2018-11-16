{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Blockchain.Keys where

import qualified Data.Aeson                      as A
import           Enecuum.Assets.System.Directory (keysFilePath)
import           Enecuum.Config
import qualified Enecuum.Core.Language           as L
import qualified Enecuum.Domain                  as D
import qualified Enecuum.Interpreters            as I
import           Enecuum.Prelude

type Password = String
data PasswordSource = Manual Password | PhraseGenerator deriving (Show, Eq, Generic, ToJSON, FromJSON)

type PublicKey = String
type PrivateKey = String
type CipheredPrivateKey = String

data ManualAppKeys = ManualAppKeys
    { _publicKey  :: PublicKey
    , _privateKey :: CipheredPrivateKey
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AutomaticAppKeys = AutomaticAppKeys
    { _publicKey  :: PublicKey
    , _privateKey :: PrivateKey
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppKeys = ManualGen ManualAppKeys | Automatic AutomaticAppKeys
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppConfig = AppConfig
    { _nodeId  :: AppKeys
    , _wallets :: [AppKeys]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

createWalletKey :: PasswordSource -> IO ()
createWalletKey = undefined

-- setAppConfig :: (L.FileSystem m, Monad m) => m AppConfig -> m ()
setAppConfig :: (Monad m, L.FileSystem m, ToJSON a) => a -> m ()
setAppConfig appConfig = do
    undefined

getAppKeys :: (L.FileSystem m, Monad m) => m AppKeys
getAppKeys = do
    appConfig <- L.readFile =<< keysFilePath
    -- A.decode appConfig
    undefined

generatePassPhrase = undefined

getPassword :: PasswordSource -> Password
getPassword (Manual password) = password
getPassword PhraseGenerator   = generatePassPhrase

encryptKey :: Password -> String -> CipheredPrivateKey
encryptKey _ key = key

-- createNodeId :: (L.FileSystem m, L.Crypto m, L.Logger m, Monad m) => PasswordSource -> m ()
createNodeId' source = do
    let password = getPassword source
    D.KeyPair publicKey privateKey <- L.evalCoreCrypto L.generateKeyPair
    let publicKeyS = D.showPublicKey publicKey
    let privateKeyS = D.showPrivateKey privateKey
    let encryptedPrivateKey = encryptKey password privateKeyS
    let nodeId = ManualAppKeys 
                         { _publicKey  = publicKeyS
                         , _privateKey = encryptedPrivateKey
                         }
    let appConfig = AppConfig { _nodeId = ManualGen nodeId, _wallets = [] } 
    fp <- keysFilePath
    L.writeFile fp (A.encode appConfig)                           
    L.logInfo $ "Your can access to private key via password: " +|| password ||+ ""

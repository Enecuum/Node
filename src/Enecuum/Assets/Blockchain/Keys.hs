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
data CreatedBy = System | User PasswordSource deriving (Show, Eq, Generic, ToJSON, FromJSON)

type PublicKey = String
type PrivateKey = String
type CipheredPrivateKey = String

type WalletAlias = String
data KeyType = Wallet WalletAlias | NodeId

data WalletWithAlias = WalletWithAlias WalletAlias AppKeyPair
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | for manually generated key pair
data ManualAppKeyPair = ManualAppKeyPair
    { _publicKey  :: PublicKey
    , _privateKey :: CipheredPrivateKey
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | for automatically generated key pair
data AutomaticAppKeyPair = AutomaticAppKeyPair
    { _publicKey  :: PublicKey
    , _privateKey :: PrivateKey
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppKeyPair = ManualGen ManualAppKeyPair | Automatic AutomaticAppKeyPair
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AppConfig = AppConfig
    { _nodeId  :: AppKeyPair
    , _wallets :: [WalletWithAlias]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


generatePassPhrase = "The quick brown fox jumps over the lazy dog"

getPassword :: PasswordSource -> Password
getPassword (Manual password) = password
getPassword PhraseGenerator   = generatePassPhrase

decryptKey :: Password -> CipheredPrivateKey -> PrivateKey
decryptKey password cipheredKey = cipheredKey

encryptKey :: Password -> String -> CipheredPrivateKey
encryptKey _ key = key


showWallets :: (L.FileSystem m, L.ERandom m, L.Logger m, Monad m) => m Text
showWallets = do
    filepath <- keysFilePath
    currentAppConfig <- (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath
    case currentAppConfig of
        Nothing -> pure ""
        Just config -> pure $ show $ _wallets config

getKeyPairNodeId :: (L.FileSystem m, L.ERandom m, L.Logger m, Monad m) => m (D.PublicKey, D.PrivateKey)
getKeyPairNodeId = do
    nodeId <- getNodeId
    case nodeId of
        ManualGen keyPair -> error "You need to have password to decrypt manually created key pair"
        Automatic keyPair-> pure (publicKey, privateKey)
                            where
                                publicKey  = D.readPublicKey  $ _publicKey  (keyPair :: AutomaticAppKeyPair)
                                privateKey = D.readPrivateKey $ _privateKey (keyPair :: AutomaticAppKeyPair)

getNodeId :: (L.FileSystem m, L.ERandom m, L.Logger m, Monad m) => m AppKeyPair
getNodeId = do
    filepath <- keysFilePath
    currentAppConfig <- (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath
    case currentAppConfig of
        Nothing -> do
            -- error $ "AppConfig doesn't exist by path: " +|| filepath ||+ "."
            createKeyPair NodeId System
            getNodeId
        Just config -> pure $ _nodeId config

createKey createdBy = do
    D.KeyPair publicKey privateKey <- L.evalCoreCrypto L.generateKeyPair
    let publicKeyS = D.showPublicKey publicKey
    let privateKeyS = D.showPrivateKey privateKey
    case createdBy of
        System -> do
            L.logInfo $ "KeyPair created automatically"
            pure $ Automatic $ AutomaticAppKeyPair
                    { _publicKey  = publicKeyS
                    , _privateKey = privateKeyS
                    }
        User source -> do
            L.logInfo $ "Your can access to private key via password: " +|| password ||+ ""
            pure $ ManualGen $ ManualAppKeyPair
                    { _publicKey  = publicKeyS
                    , _privateKey = encryptedPrivateKey
                    }
                where password = getPassword source
                      encryptedPrivateKey = encryptKey password privateKeyS

createKeyPair
  :: (Monad m, L.ERandom m, L.Logger m, L.FileSystem m) =>
    KeyType -> CreatedBy -> m ()
createKeyPair keyType createdBy = do
    keyPair <- createKey createdBy
    filepath <- keysFilePath
    currentAppConfig <- (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath

    let appConfig = case keyType of
            NodeId -> case currentAppConfig of
                Nothing -> AppConfig { _nodeId = keyPair, _wallets = [] }
                Just config -> config { _nodeId = keyPair }
            Wallet walletAlias -> case currentAppConfig of
                Nothing -> error $ "There are no app config yet at " +|| filepath ||+ ""
                Just config -> config { _wallets = wallets }
                               where wallets = (WalletWithAlias walletAlias keyPair) : (_wallets config)

    L.logInfo $ "" +|| (A.encode appConfig) ||+ ""
    L.writeFile filepath $ A.encode appConfig


createWallet' walletAlias createdBy = createKeyPair (Wallet walletAlias) createdBy

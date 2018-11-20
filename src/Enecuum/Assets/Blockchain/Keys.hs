{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Enecuum.Assets.Blockchain.Keys where

import qualified Data.Aeson                      as A
import           Data.ByteString.Extra           ()
import           Enecuum.Assets.System.Directory (keysFilePath)
import qualified Enecuum.Domain                  as D
import qualified Enecuum.Language                as L
import           Enecuum.Prelude

type Password = String
data PasswordSource = Manual Password | PhraseGenerator deriving (Show, Eq, Generic, ToJSON, FromJSON)
data CreatedBy = System | User PasswordSource deriving (Show, Eq, Generic, ToJSON, FromJSON)

type PublicKey = String
type PrivateKey = ByteString
type CipheredPrivateKey = ByteString

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

-- | sum type for key pair (key pair can be produced automatically or manually)
data AppKeyPair = ManualGen ManualAppKeyPair | Automatic AutomaticAppKeyPair
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | data type for permanent storage
data AppConfig = AppConfig
    { _nodeId  :: AppKeyPair
    , _wallets :: [WalletWithAlias]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


generatePassPhrase = "The quick brown fox jumps over the lazy dog"

getPassword :: PasswordSource -> Password
getPassword (Manual password) = password
getPassword PhraseGenerator   = generatePassPhrase

-- | Decrypt private key
decryptKey :: (Monad m, L.Crypto m) => Password -> CipheredPrivateKey -> m PrivateKey
decryptKey password cipheredKey = L.decrypt (fromString password) cipheredKey

-- | Encrypt private key
encryptKey :: (Monad m, L.Crypto m) => Password -> PrivateKey -> m CipheredPrivateKey
encryptKey password msg = L.encrypt (fromString password) msg

-- | Show stored wallets
showWallets :: L.NodeL Text
showWallets = do
    filepath <- keysFilePath
    currentAppConfig <- (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath
    case currentAppConfig of
        Nothing     -> pure ""
        Just config -> pure $ show $ intersperse "\n" $ map showWallet $ _wallets config

showWallet :: WalletWithAlias -> Text
showWallet (WalletWithAlias alias (Automatic keyPair) ) = "" +|| alias ||+ " " +|| _publicKey (keyPair :: AutomaticAppKeyPair) ||+ ""
showWallet (WalletWithAlias alias (ManualGen keyPair) ) = "" +|| alias ||+ " " +|| _publicKey (keyPair :: ManualAppKeyPair)    ||+ ""

-- | Get public and private key of NodeId
getKeyPairNodeId :: (ToString PrivateKey) => L.NodeL (D.PublicKey, D.PrivateKey)
getKeyPairNodeId = do
    nodeId <- getNodeId
    case nodeId of
        ManualGen keyPair -> error "You need to have password to decrypt manually created key pair"
        Automatic keyPair-> pure (publicKey, privateKey)
                            where
                                publicKey  = D.readPublicKey  $ toString $ _publicKey  (keyPair :: AutomaticAppKeyPair)
                                privateKey = D.readPrivateKey $ toString $ _privateKey (keyPair :: AutomaticAppKeyPair)

-- | Get NodeId key pait in stored format
getNodeId :: L.NodeL AppKeyPair
getNodeId = do
    filepath <- keysFilePath
    currentAppConfig <- (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath
    case currentAppConfig of
        Nothing -> do
            -- error $ "AppConfig doesn't exist by path: " +|| filepath ||+ "."
            createKeyPair NodeId System
            getNodeId
        Just config -> pure $ _nodeId config

-- | Create key pair by system or by user
createKey :: CreatedBy -> L.NodeL AppKeyPair
createKey createdBy = do
    D.KeyPair publicKey privateKey <- L.evalCoreCrypto L.generateKeyPair
    let publicKeyS = fromString $ D.showPublicKey publicKey
    let privateKeyS = fromString $ D.showPrivateKey privateKey
    case createdBy of
        System -> do
            L.logInfo $ "KeyPair created automatically"
            pure $ Automatic $ AutomaticAppKeyPair
                    { _publicKey  = publicKeyS
                    , _privateKey = privateKeyS
                    }
        User source -> do
            let password = getPassword source
            L.logInfo $ "Your can access to private key via password: " +|| password ||+ ""
            encryptedPrivateKey <- L.evalCoreCrypto $ encryptKey password privateKeyS
            pure $ ManualGen $ ManualAppKeyPair
                    { _publicKey  = publicKeyS
                    , _privateKey = encryptedPrivateKey
                    }

-- | Create key pair by system or by user and write to file
createKeyPair :: KeyType -> CreatedBy -> L.NodeL ()
createKeyPair keyType createdBy = do
    keyPair <- createKey createdBy
    filepath <- keysFilePath
    isFileExist <- L.doesFileExist filepath
    currentAppConfig <- if isFileExist
        then (\text -> A.decode text :: Maybe AppConfig) <$> L.readFile filepath
        else pure Nothing

    let appConfig = case keyType of
            NodeId -> case currentAppConfig of
                Nothing     -> AppConfig { _nodeId = keyPair, _wallets = [] }
                Just config -> config { _nodeId = keyPair }
            Wallet walletAlias -> case currentAppConfig of
                Nothing -> error $ "There are no app config yet at " +|| filepath ||+ ""
                Just config -> config { _wallets = wallets }
                               where wallets = (WalletWithAlias walletAlias keyPair) : (_wallets config)

    L.writeFile filepath $ A.encode appConfig

-- | Create wallet with alias (can be created by system or by user)
createWallet' :: WalletAlias -> CreatedBy -> L.NodeL ()
createWallet' walletAlias createdBy = createKeyPair (Wallet walletAlias) createdBy

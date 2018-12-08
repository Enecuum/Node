{-# LANGUAGE DeriveAnyClass         #-}
module Enecuum.Samples.Assets.Blockchain.Wallet where

import qualified Enecuum.Samples.Blockchain.Domain as D   
import Enecuum.Prelude

-- | Wallets and keys for demo purpose

publicKeys :: [D.PublicKey]
publicKeys = map D.readPublicKey
    [ "8fM3up1pPDUgMnYZzKiBpsnrvNopjSoURSnpYbm5aZKz"
    , "4vCovnpyuooGBi7t4LcEGeiQYA2pEKc4hixFGRGADw4X"
    , "GS5xDwfTffg86Wyv8uy3H4vVQYqTXBFKPxGPy1Ksp2NS"
    , "Jh8vrASby8nrVG7N3PLZjqSpbrpXFGmfpMd1nrYifZou"
    , "8LZQhs3Z7WiBZbQvTTeXCcCtXfJYtk6RNxxBExo9PEQm"
    ]

privateKeys :: [D.PrivateKey]
privateKeys = map D.readPrivateKey
    [ "FDabUqrGEd1i3rfZpqHJkzhvqP9QEpKveoEwmknfJJFa"
    , "DKAJTFr1bFWHE7psYX976YZis1Fqwkh3ikFAgKaw6bWj"
    , "6uU38xA2ucJ2zEqgg1zs5j3U8hx8RL3thVFNmhk3Nbsq"
    , "3n8QPsZwUJxUK85VrgTEuybyj1zDnUeMeovntB5EdqWP"
    , "MzwHKfF4vGsQB2hgcK3MFKY9TaFaUe78NJwQehfjZ5s"
    ]

hardcodedWallets :: [D.KeyPair]
hardcodedWallets = uncurry D.KeyPair <$> zip publicKeys privateKeys

type ClientName = String

names :: [ClientName]
names = ["me", "Alice", "Bob", "Carol", "David"]

hardcodedWalletsWithNames :: [CLIWallet]
hardcodedWalletsWithNames = [ CLIWallet {_id  =  walletId, _name = name, _publicKey = pub, _privateKey = Just priv} | walletId <- [1..], name <- names, pub <- publicKeys]
    where priv = head privateKeys

data CLIWallet = CLIWallet
    { _id         :: Int
    , _name       :: ClientName
    , _publicKey  :: D.PublicKey
    , _privateKey :: Maybe D.PrivateKey
    } deriving (Generic, Show, Eq, Ord, Read, ToJSON, FromJSON, Serialize)


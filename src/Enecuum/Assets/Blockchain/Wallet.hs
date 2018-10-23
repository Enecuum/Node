module Enecuum.Assets.Blockchain.Wallet where

import Enecuum.Blockchain.Domain.Crypto
import Enecuum.Prelude

-- | Wallets and keys for demo purpose

-- | Keys that are hardcoded in keys.txt.
publicKeys :: [PublicKey]
publicKeys = map readPublicKey
    [ "8fM3up1pPDUgMnYZzKiBpsnrvNopjSoURSnpYbm5aZKz"
    , "4vCovnpyuooGBi7t4LcEGeiQYA2pEKc4hixFGRGADw4X"
    , "GS5xDwfTffg86Wyv8uy3H4vVQYqTXBFKPxGPy1Ksp2NS"
    , "Jh8vrASby8nrVG7N3PLZjqSpbrpXFGmfpMd1nrYifZou"
    , "8LZQhs3Z7WiBZbQvTTeXCcCtXfJYtk6RNxxBExo9PEQm"
    ]

privateKeys :: [PrivateKey]
privateKeys = map readPrivateKey
    [ "FDabUqrGEd1i3rfZpqHJkzhvqP9QEpKveoEwmknfJJFa"
    , "DKAJTFr1bFWHE7psYX976YZis1Fqwkh3ikFAgKaw6bWj"
    , "6uU38xA2ucJ2zEqgg1zs5j3U8hx8RL3thVFNmhk3Nbsq"
    , "3n8QPsZwUJxUK85VrgTEuybyj1zDnUeMeovntB5EdqWP"
    , "MzwHKfF4vGsQB2hgcK3MFKY9TaFaUe78NJwQehfjZ5s"
    ]

hardcodedWallets :: [KeyPair]
hardcodedWallets = map (\(pub, priv) -> KeyPair pub priv) $ zip publicKeys privateKeys

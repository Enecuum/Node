{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Enecuum.Tests.Integration.DatabaseSpec where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.List as List
import           Control.Lens             ( makeFieldsNoPrefix )

import           Enecuum.Prelude
import qualified Enecuum.Domain as D
import qualified Enecuum.Language as L

import           Test.Hspec


data KBlocksMetaDB
data KBlockMetaEntity = KBlockMetaEntity D.DBIndex
type KBlockMetaEntityKey = D.DBKey KBlocksMetaDB KBlockMetaEntity

data KBlocksDB
data KBlockPrevHashEntity = KBlockPrevHashEntity D.StringHash
type KBlockPrevHashEntityKey = D.DBKey KBlocksDB KBlockPrevHashEntity

data KBlockEntity = KBlockEntity
    { _time      :: Integer
    , _number    :: Integer
    , _nonce     :: Integer
    , _solver    :: D.StringHash
    }
type KBlockEntityKey = D.DBKey KBlocksDB KBlockEntity


-- findValue :: forall a m. (Typeable a, FromJSON a, Database m) => D.DBKey -> m (Either D.DBError a)
-- findValue key = do
--     mbRaw <- getValue key
--     case mbRaw of
--         Nothing  -> pure $ Left $ D.KeyNotFound key
--         Just raw -> case A.decode raw of
--             Nothing  -> pure $ Left $ D.InvalidType $ show $ typeOf (Proxy :: Proxy a)
--             Just val -> pure $ Right val


findValue :: D.DBKey db spec -> L.DatabaseL db (Maybe a)
findValue key = undefined

toDBKey :: a -> D.DBKey db spec
toDBKey = undefined


data NodeData = NodeData
    { _kBlocksDB     :: D.Storage KBlocksDB
    , _kBlocksMetaDB :: D.Storage KBlocksMetaDB
    }

makeFieldsNoPrefix ''NodeData

getKBlockMeta :: NodeData -> D.KBlock -> L.NodeL (Maybe KBlockMetaEntity)
getKBlockMeta nodeData kBlock = do
    let key :: KBlockMetaEntityKey = toDBKey kBlock
    L.withDatabase (nodeData ^. kBlocksMetaDB) $ findValue key

toKBlock :: KBlockPrevHashEntity -> KBlockEntity -> D.KBlock
toKBlock (KBlockPrevHashEntity prevHash) (KBlockEntity t n nc s) = D.KBlock
    { D._time     = t
    , D._prevHash = prevHash
    , D._number   = n
    , D._nonce    = nc
    , D._solver   = s
    }

getKBlock :: NodeData -> Maybe KBlockMetaEntity -> L.NodeL (Maybe D.KBlock)
getKBlock nodeData Nothing = pure Nothing
getKBlock nodeData (Just e) = do
    let key1 :: KBlockPrevHashEntityKey = toDBKey e
    let key2 :: KBlockEntityKey         = toDBKey e
    mbPrevHashEntity <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key1
    mbKBlockEntity   <- L.withDatabase (nodeData ^. kBlocksDB) $ findValue key2
    pure $ toKBlock <$> mbPrevHashEntity <*> mbKBlockEntity


getNextKBlock :: NodeData -> D.KBlock -> L.NodeL (Maybe D.KBlock)
getNextKBlock nodeData kBlock = do
    mbMeta   <- getKBlockMeta nodeData kBlock
    mbKBlock <- getKBlock nodeData mbMeta
    pure Nothing

kBlock1 = D.KBlock
    { _time      = 0
    , _prevHash  = D.genesisHash
    , _number    = 1
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock2 = D.KBlock
    { _time      = 0
    , _prevHash  = D.toHash kBlock1
    , _number    = 2
    , _nonce     = 0
    , _solver    = D.genesisHash
    }

kBlock3 = D.KBlock
    { _time      = 0
    , _prevHash  = D.toHash kBlock2
    , _number    = 3
    , _nonce     = 0
    , _solver    = D.genesisHash
    }


spec :: Spec
spec = describe "Database support tests" $ do
    it "Test 1" $ "failed" `shouldBe` "1"
    it "Test 2" $ do
        print $ D.genesisHash
        print $ D.toHash kBlock1
        print $ D.toHash kBlock2
        print $ D.toHash kBlock3

        "failed" `shouldBe` "1"

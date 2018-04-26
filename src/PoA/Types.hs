{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, FlexibleInstances #-}
module PoA.Types where

import              Data.Word()
import qualified    Data.ByteString as B
import              Data.Aeson
import              Data.String
import qualified    Data.Text.Lazy as T
import              Data.Hex
import              Control.Monad.Extra
import              Data.Either
import qualified    Data.Serialize as S
import              Sharding.Space.Point
import              Service.Types (Microblock(..), Transaction)
import              Service.Network.Base (HostAddress, PortNumber)
import              Data.IP

-- TODO: aception of msg from a PoA/PoW.
-- ----: parsing - ok!
-- TODO: processing of the msg
-- TODO:    Resending (to point);
-- TODO:    Broadcasting (in net);
-- TODO:    Response.

-- TODO: i have msg (it not responce) for PoA/PoW node.
-- ----     toJson
-- TODO sending to PoA/PoW node.

-- TODO finding of optimal broadcast node for PoA/PoW node. ???

newtype UUID = UUID Point

data PoAPoWToNnMessage
    -- Запросы:
    -- на получение сообщения от PoA/PoW ноды.
    = RequestMsgTo { ---
        sender :: UUID
    }

    -- на получение транзакций.
    | RequestTransaction { ---
        number :: Int
    }

    -- запрос на рассылку бродкаста.
    | RequestBroadcast { ---
        msg :: B.ByteString
    }
    -- запрос на получение конектов.
    | RequestConnects
    | RequestUUIDToNn

    -- Ответы с UUID
    | ResponseUUIDToNn {
        uuid :: UUID
    }

    -- Сообщения:
    -- Для другой PoA/PoW ноды.
    | MsgMsgTo { ----
        destination :: UUID,
        msg :: B.ByteString
    }

    -- О том, что намйнился микроблок.
    | MsgMicroblock {
        microblock :: Microblock
    }

data NodeType = PoW | PoA deriving Show


data Connect = Connect HostAddress PortNumber

data NnToPoAPoWMessage
    = RequestUUIDToPoAPoWMessage

    | ResponseMsgTo {
        sender :: UUID,
        messages :: [B.ByteString]
    }

    | ResponseUUIDToPoAPoWMessage {
        uuid :: UUID
    }

    | ResponseConnects {
      connects  :: [Connect]
    }

    | MsgConnect {
        ip    :: HostAddress,
        port  :: PortNumber
    }

    | MsgNewNodeInNet {
        id :: UUID,
        nodeType :: NodeType
    }

    | ResponseTransaction {
        transaction :: Transaction
    }

    | ResponseBroadcastMsg {
        messages :: [B.ByteString]
    }


myUnhex :: (MonadPlus m, S.Serialize a) => T.Text -> m a
myUnhex aString = case unhex $ T.unpack $ aString of
    Just aDecodeString  -> case S.decode $ fromString $ aDecodeString of
        Right aJustVal  -> return aJustVal
        Left _          -> mzero
    Nothing             -> mzero

myTextUnhex :: T.Text -> Maybe B.ByteString
myTextUnhex aString = fromString <$> aUnxeded
    where
        aUnxeded :: Maybe String
        aUnxeded = unhex aNewString

        aNewString :: String
        aNewString = T.unpack aString

instance FromJSON PoAPoWToNnMessage where
    parseJSON (Object aMessage) = do
        aTag  :: T.Text <- aMessage .: "tag"
        aType :: T.Text <- aMessage .: "type"
        case (T.unpack aTag, T.unpack aType) of
            ("Request", "MsgTo") -> do
                aSender :: T.Text <- aMessage .: "sender"
                aPoint <- myUnhex aSender
                return (RequestMsgTo $ UUID aPoint)

            ("Request", "Transaction") -> RequestTransaction <$> aMessage .: "number"

            ("Request", "Broadcast") -> do
                aMsg :: T.Text <- aMessage .: "msg"
                case myTextUnhex aMsg of
                    Just aUnxededMsg  -> return $ RequestBroadcast aUnxededMsg
                    Nothing           -> mzero

            ("Request","Connects")    -> return RequestConnects

            ("Request","UUID")        -> return RequestUUIDToNn

            ("Response", "UUID") -> do
                aUuid :: T.Text <- aMessage .: "uuid"
                aPoint <- myUnhex aUuid
                return (ResponseUUIDToNn $ UUID aPoint)

            ("Msg", "MsgTo") -> do
                aDestination :: T.Text <- aMessage .: "destination"
                aMsg         :: T.Text <- aMessage .: "msg"
                aPoint <- myUnhex aDestination
                case myTextUnhex aMsg of
                    Just aJustMsg -> return $ MsgMsgTo (UUID aPoint) aJustMsg
                    Nothing -> mzero


            ("Msg", "Microblock") -> do
                aPreviousHash :: T.Text <- aMessage .: "previousHash"
                aBlockHash    :: T.Text <- aMessage .: "blockHash"
                aListTransaction  <- aMessage .: "transactions"
                case (myTextUnhex aPreviousHash, myTextUnhex aBlockHash) of
                    (Just aHash1, Just aHash2) ->
                        case (decodeList aListTransaction) of
                            []      -> mzero
                            aResult -> return . MsgMicroblock
                                $ Microblock aHash1 aHash2 aResult
                    _   -> mzero


            _ -> mzero

    parseJSON _ = mzero


decodeList :: S.Serialize a => [T.Text] -> [a]

decodeList aList
    | all isRight aDecodeList   = rights aDecodeList
    | otherwise                 = []
    where aDecodeList = S.decode <$> fromString . T.unpack <$> aList


instance ToJSON NnToPoAPoWMessage where
    toJSON RequestUUIDToPoAPoWMessage = object [
        "tag"   .= ("Request" :: String),
        "type"  .= ("UUID"    :: String)
      ]

    toJSON (ResponseMsgTo aUuid aMessages) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("MsgTo"     :: String),
        "sender"    .= uuidToString aUuid,
        "messages"  .= (show.hex <$> aMessages)
      ]

    toJSON (ResponseUUIDToPoAPoWMessage aUuid) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("UUID"      :: String),
        "uuid"      .= uuidToString aUuid
      ]

    toJSON (ResponseConnects aConnects) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("Connects"  :: String),
        "connects"  .= aConnects
      ]

    toJSON (MsgConnect aIp aPort) = toJSON $ Connect aIp aPort

    toJSON (MsgNewNodeInNet aUuid aNodeType) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("NewNodeInNet"  :: String),
        "id"        .= uuidToString aUuid,
        "nodeType"  .= show aNodeType
      ]

    toJSON (ResponseTransaction aTransaction) = object [
        "transaction" .= (hex . show $ S.encode aTransaction)
      ]

    toJSON (ResponseBroadcastMsg aMessages) = object [
        "messages"  .= (show.hex <$> aMessages)
      ]


instance ToJSON Connect where
    toJSON (Connect aHostAddress aPortNumber) = object [
        "ip"   .= show (fromHostAddress aHostAddress),
        "port" .= fromEnum aPortNumber
      ]

uuidToString :: UUID -> String
uuidToString (UUID aPoint) = show . hex $ S.encode aPoint


--------------------------------------------------------------------------------

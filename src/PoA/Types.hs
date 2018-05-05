{-# LANGUAGE
        OverloadedStrings
    ,   ScopedTypeVariables
    ,   DuplicateRecordFields
    ,   FlexibleInstances
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
  #-}
module PoA.Types where

import              Data.Word()
import qualified    Data.ByteString as B
import              Data.Aeson
import              Data.String
import              GHC.Generics
import qualified    Data.Text.Lazy as T
import              Data.Hex
import              Control.Monad.Extra
import              Data.Either
import qualified    Data.Serialize as S
import              Sharding.Space.Point
import              Service.Types (Microblock(..), Transaction)
import              Service.Network.Base
import              Data.IP

-- TODO: aception of msg from a PoA/PoW.
-- ----: parsing - ok!
-- TODO: processing of the msg
-- TODO:    Resending (to point);
-- TODO:    Broadcasting (in net);
-- TODO:    Response.

-- TODO: i have msg (it not response) for PoA/PoW node.
-- ----     toJson
-- TODO sending to PoA/PoW node.

-- TODO finding of optimal broadcast node for PoA/PoW node. ???

newtype UUID    = UUID    Point deriving (Show, Ord, Eq, Generic, S.Serialize)
newtype IdFrom  = IdFrom  UUID  deriving (Show, Ord, Eq, Generic, S.Serialize)
newtype IdTo    = IdTo    UUID  deriving (Show, Ord, Eq, Generic, S.Serialize)


data PPToNNMessage
    -- Запросы:
    -- на получение транзакций.
    = RequestTransaction { ---
        number :: Int
    }

    -- запрос на получение списка PoW нод
    | RequestPoWList

    -- запрос на рассылку бродкаста.
    | RequestBroadcast { ---
        recipientType :: NodeType,
        msg           :: B.ByteString
    }
    -- запрос на получение конектов.
    | RequestConnects

    -- Ответы с UUID
    | ResponseNodeIdToNN {
        nodeId    :: UUID,
        nodeType  :: NodeType
    }

    -- Сообщения:
    -- Для другой PoA/PoW ноды.
    | MsgMsgToNN { ----
        destination :: UUID,
        msg :: B.ByteString
    }

    -- О том, что намйнился микроблок.
    | MsgMicroblock {
        microblock :: Microblock
    }


    -- О том, что закрылся ма кроблок.
    -- | MsgMacroblock {
    --     macroblock :: Macroblock
    -- }

data NodeType = PoW | PoA deriving (Eq, Show, Ord, Generic)

instance S.Serialize NodeType



-- PP means PoW and PoA
-- MsgToMainActorFromPP

data NNToPPMessage
    = RequestNodeIdToPP

    | ResponseConnects {
      connects  :: [Connect]
    }

    | ResponseTransaction {
        transaction :: Transaction
    }

    -- ответ со списком PoW нод
    | ResponsePoWList {
        poWList :: [UUID]
    }

    | MsgConnect {
        ip    :: HostAddress,
        port  :: PortNumber
    }

    | MsgMsgToPP {
        sender :: UUID,
        message :: B.ByteString
    }

    | MsgBroadcastMsg {
        message :: B.ByteString,
        idFrom  :: IdFrom
    }

    | MsgNewNodeInNet {
        id :: UUID,
        nodeType :: NodeType
    }
{-

{
    "tag": "Response",
    "type": "PoWList",
    "poWList": [UUID]
}
-}

myUnhex :: (MonadPlus m, S.Serialize a) => T.Text -> m a
myUnhex aString = case unhex $ T.unpack aString of
    Just aDecodeString  -> case S.decode $ fromString aDecodeString of
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

instance FromJSON PPToNNMessage where
    parseJSON (Object aMessage) = do
        aTag  :: T.Text <- aMessage .: "tag"
        aType :: T.Text <- aMessage .: "type"
        case (T.unpack aTag, T.unpack aType) of
            ("Request", "Transaction") -> RequestTransaction <$> aMessage .: "number"

            ("Request", "Broadcast") -> do
                aMsg :: T.Text <- aMessage .: "msg"
                aRecipientType :: T.Text <-  aMessage .: "aRecipientType"
                case myTextUnhex aMsg of
                    Just aUnxededMsg  -> return $
                        RequestBroadcast (readNodeType aRecipientType) aUnxededMsg
                    Nothing           -> mzero

            ("Request","Connects")    -> return RequestConnects
            ("Request","PoWList")     -> return RequestPoWList

            ("Response", "NodeId") -> do
                aUuid :: T.Text <- aMessage .: "nodeId"
                aPoint    <- myUnhex aUuid
                aNodeType :: T.Text <- aMessage .: "nodeType"
                return (ResponseNodeIdToNN (UUID aPoint) (readNodeType aNodeType))

            ("Msg", "MsgTo") -> do
                aDestination :: T.Text <- aMessage .: "destination"
                aMsg         :: T.Text <- aMessage .: "msg"
                aPoint <- myUnhex aDestination
                case myTextUnhex aMsg of
                    Just aJustMsg -> return $ MsgMsgToNN (UUID aPoint) aJustMsg
                    Nothing -> mzero

            ("Msg", "Microblock") -> do
                aPreviousHash :: T.Text <- aMessage .: "previousHash"
                aBlockHash    :: T.Text <- aMessage .: "blockHash"
                aListTransaction  <- aMessage .: "transactions"
                case (myTextUnhex aPreviousHash, myTextUnhex aBlockHash) of
                    (Just aHash1, Just aHash2) ->
                        case decodeList aListTransaction of
                            []      -> mzero
                            aResult -> return . MsgMicroblock
                                $ Microblock aHash1 aHash2 aResult
                    _   -> mzero


            _ -> mzero

    parseJSON _ = mzero

readNodeType :: (IsString a, Eq a) => a -> NodeType
readNodeType aNodeType = if aNodeType == "PoW" then PoW else PoA

decodeList :: S.Serialize a => [T.Text] -> [a]

decodeList aList
    | all isRight aDecodeList   = rights aDecodeList
    | otherwise                 = []
    where aDecodeList = S.decode . fromString . T.unpack <$> aList


instance ToJSON NNToPPMessage where
    toJSON RequestNodeIdToPP = object [
        "tag"   .= ("Request" :: String),
        "type"  .= ("NodeId"  :: String)
      ]

    toJSON (MsgMsgToPP aUuid aMessage) = object [
        "tag"       .= ("Msg"   :: String),
        "type"      .= ("MsgTo" :: String),
        "sender"    .= uuidToString aUuid,
        "messages"  .= (show.hex $ aMessage)
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

    toJSON (MsgBroadcastMsg aMessage (IdFrom aUuid)) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("BroadcastMsg"  :: String),
        "messages"  .= (show.hex $ aMessage),
        "idFrom"    .= uuidToString aUuid
      ]

    toJSON (ResponsePoWList aUUIDs) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("PoWList"   :: String),
        "poWList"   .=  map uuidToString aUUIDs
      ]

instance ToJSON Connect where
    toJSON (Connect aHostAddress aPortNumber) = object [
        "ip"   .= show (fromHostAddress aHostAddress),
        "port" .= fromEnum aPortNumber
      ]



uuidToString :: UUID -> String
uuidToString (UUID aPoint) = show . hex $ S.encode aPoint


--------------------------------------------------------------------------------

{-# LANGUAGE
        OverloadedStrings
    ,   ScopedTypeVariables
    ,   DuplicateRecordFields
    ,   FlexibleInstances
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   StandaloneDeriving
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module PoA.Types where

import              Data.Word()
import qualified    Data.ByteString as B
import qualified    Data.ByteString.Char8 as CB
import              Data.Aeson
import              Data.String
import              GHC.Generics
import qualified    Data.Text as T
import              Data.Hex
import              Control.Monad.Extra
import              Data.Either
import qualified    Data.Serialize as S
import              Service.Types (Microblock(..), Transaction)
import              Service.Network.Base
import              Data.IP
import              Node.Data.Key
import              Service.Types.SerializeInstances
import qualified    Data.HashMap.Strict as H
import qualified    Data.Vector as V
import              Data.Scientific

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

instance S.Serialize Scientific where
    get = read <$> S.get
    put = S.put . show


instance S.Serialize T.Text where
    get = T.pack <$> S.get
    put = S.put . T.unpack

instance S.Serialize Object where
    get = fmap H.fromList S.get
    put = S.put . H.toList


instance S.Serialize a => S.Serialize (V.Vector a) where
    get = fmap V.fromList S.get
    put = S.put . V.toList

instance S.Serialize Value


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

    -- Ответы с PPId
    | ResponseNodeIdToNN {
        nodeId    :: PPId,
        nodeType  :: NodeType
    }

    -- Сообщения:
    -- Для другой PoA/PoW ноды.
    | MsgMsgToNN { ----
        destination :: PPId,
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
    deriving (Show)

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
        poWList :: [PPId]
    }

    | MsgConnect {
        ip    :: HostAddress,
        port  :: PortNumber
    }

    | MsgMsgToPP {
        sender :: PPId,
        message :: B.ByteString
    }

    | MsgBroadcastMsg {
        message :: B.ByteString,
        idFrom  :: IdFrom
    }

    | MsgNewNodeInNet {
        id :: PPId,
        nodeType :: NodeType
    }


myUnhex :: (MonadPlus m, S.Serialize a) => T.Text -> m a
myUnhex aString = case unhex $ T.unpack aString of
    Just aDecodeString  -> do
        case S.decode $ fromString aDecodeString of
            Right aJustVal  -> return aJustVal
            Left _          -> mzero
    Nothing             -> mzero


unhexNodeId :: MonadPlus m => T.Text -> m NodeId
unhexNodeId aString = case unhex . fromString . T.unpack $ aString of
    Just aDecodeString  -> return . NodeId . roll $ B.unpack aDecodeString
    Nothing             -> mzero


ppIdToString :: PPId -> String
ppIdToString (PPId (NodeId aPoint)) = CB.unpack . hex . B.pack $ unroll aPoint


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
        --error $ show aTag ++ " " ++ show aType
        case (T.unpack aTag, T.unpack aType) of
            ("Request", "Transaction") -> RequestTransaction <$> aMessage .: "number"

            ("Request", "Broadcast") -> do
                aMsg :: Value <- aMessage .: "msg"
                aRecipientType :: T.Text <-  aMessage .: "recipientType"
                return $ RequestBroadcast (readNodeType aRecipientType) (S.encode aMsg)

            ("Request","Connects")    -> return RequestConnects
            ("Request","PoWList")     -> return RequestPoWList

            ("Response", "NodeId") -> do
                aPPId :: T.Text <- aMessage .: "nodeId"

                aPoint   <- unhexNodeId aPPId
                aNodeType :: T.Text <- aMessage .: "nodeType"
                return (ResponseNodeIdToNN (PPId aPoint) (readNodeType aNodeType))

            ("Msg", "MsgTo") -> do
                aDestination :: T.Text <- aMessage .: "destination"
                aMsg         :: Value  <- aMessage .: "msg"
                aPoint <- unhexNodeId aDestination
                return $ MsgMsgToNN (PPId aPoint) (S.encode aMsg)

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

    parseJSON _ = mzero -- error $ show a

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

    toJSON (MsgMsgToPP aPPId aMessage) = object [
            "tag"       .= ("Msg"   :: String),
            "type"      .= ("MsgTo" :: String),
            "sender"    .= ppIdToString aPPId,
            "messages"  .= aObj
          ]
      where
        aObj = case S.decode aMessage of
            Right (aObjValue :: Value)   -> aObjValue
            Left aObjValue               -> String (T.pack aObjValue)

    toJSON (ResponseConnects aConnects) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("Connects"  :: String),
        "connects"  .= aConnects
      ]

    toJSON (MsgConnect aIp aPort) = toJSON $ Connect aIp aPort

    toJSON (MsgNewNodeInNet aPPId aNodeType) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("NewNodeInNet"  :: String),
        "id"        .= ppIdToString aPPId,
        "nodeType"  .= show aNodeType
      ]

    toJSON (ResponseTransaction aTransaction) = object [
        "tag"       .= ("Response"     :: String),
        "type"      .= ("Transaction"  :: String),
        "transaction" .= (hex . show $ S.encode aTransaction)
      ]

    toJSON (MsgBroadcastMsg aMessage (IdFrom aPPId)) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("BroadcastMsg"  :: String),
        "messages"  .= aObj,
        "idFrom"    .= ppIdToString aPPId
      ]
      where
        aObj = case S.decode aMessage of
          Right (aObjValue :: Value)   -> aObjValue
          Left aObjValue               -> String (T.pack aObjValue)


    toJSON (ResponsePoWList aPPIds) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("PoWList"   :: String),
        "poWList"   .=  map ppIdToString aPPIds
      ]

instance ToJSON Connect where
    toJSON (Connect aHostAddress aPortNumber) = object [
        "ip"   .= show (fromHostAddress aHostAddress),
        "port" .= fromEnum aPortNumber
      ]


--------------------------------------------------------------------------------

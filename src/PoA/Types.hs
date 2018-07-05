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
import              Data.Maybe
import              Control.Monad.Extra
import qualified    Data.Serialize as S
import              Service.Types (Microblock(..), Transaction)
import              Service.Network.Base
import              Data.IP
import              Node.Data.Key
import              Service.Types.SerializeJSON()
import              Service.Types.SerializeInstances
import qualified    Data.HashMap.Strict as H
import qualified    Data.Vector as V
import              Data.Scientific
import              Data.Either
import              Text.Read

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
    -- Requests:
    -- transactions receiving.
    = RequestTransaction { ---
        number :: Int
    }

    -- receive PoW nodes' list
    | RequestPoWList

    -- send broadcast
    | RequestBroadcast { ---
        recipientType :: NodeType,
        msg           :: Value
    }
    -- get connects
    | RequestConnects Bool

    -- responses with PPId
    | ResponseNodeIdToNN NodeId NodeType

    -- Messages:
    -- For other PoA/PoW node.
    | MsgMsgToNN { ----
        destination :: NodeId,
        msg :: Value
    }

    -- new microblock was mined.
    | MsgMicroblock {
        microblock :: Microblock
    }

    | ActionNodeStillAliveTest PortNumber HostAddress
    | IsInPendingRequest Transaction
    | GetPendingRequest
    | AddTransactionRequest Transaction
    | ActionAddToListOfConnects Int

    deriving (Show)

data NodeType = PoW | PoA | All | NN deriving (Eq, Show, Ord, Generic)

instance S.Serialize NodeType

-- PP means PoW and PoA
-- MsgToMainActorFromPP

data NNToPPMessage
    = RequestNodeIdToPP

    | ResponseConnects [Connect]
    | ResponseTransaction {
        transaction :: Transaction
    }

    -- request with PoW's list
    | ResponsePoWList {
        poWList :: [NodeId]
    }

    | MsgConnect {
        ip    :: HostAddress,
        port  :: PortNumber
    }

    | MsgMsgToPP {
        sender :: NodeId,
        message :: Value
    }

    | MsgBroadcastMsg {
        message :: Value,
        idFrom  :: IdFrom
    }

    | MsgNewNodeInNet NodeId NodeType
    | ResponsePendingTransactions [Transaction]
    | ResponseIsInPending Bool
    | ResponseTransactionValid Bool


myUnhex :: IsString a => T.Text -> Either a String
myUnhex aString = case unhex $ T.unpack aString of
    Just aDecodeString  -> Right aDecodeString
    Nothing             -> Left "Nothing"


unhexNodeId :: MonadPlus m => T.Text -> m NodeId
unhexNodeId aString = case unhex . fromString . T.unpack $ aString of
    Just aDecodeString  -> return . NodeId . roll $ B.unpack aDecodeString
    Nothing             -> mzero


ppIdToString :: NodeId -> String
ppIdToString (NodeId aPoint) = CB.unpack . hex . B.pack $ unroll aPoint


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
                return $ RequestBroadcast (readNodeType aRecipientType) aMsg

            ("Request","Connects")    -> do
                aFull :: Maybe T.Text <- aMessage .:? "full"
                return $ RequestConnects (isJust aFull)

            ("Request","PoWList")     -> return RequestPoWList

            ("Response", "NodeId") -> do
                aPPId :: T.Text <- aMessage .: "nodeId"

                aNodeId   <- unhexNodeId aPPId
                aNodeType :: T.Text <- aMessage .: "nodeType"
                return (ResponseNodeIdToNN aNodeId (readNodeType aNodeType))

            ("Msg", "MsgTo") -> do
                aDestination :: T.Text <- aMessage .: "destination"
                aMsg         :: Value  <- aMessage .: "msg"
                aNodeId <- unhexNodeId aDestination
                return $ MsgMsgToNN aNodeId aMsg

            ("Msg", "Microblock") ->
                MsgMicroblock <$> aMessage .: "microblock"

            -- testing functions!!!
            ("Request", "Pending") -> do
                aMaybeTransaction <- aMessage .:? "transaction"
                return $ case aMaybeTransaction of
                    Just aTransaction -> IsInPendingRequest aTransaction
                    Nothing           -> GetPendingRequest

            ("Request", "PendingAdd") ->
                AddTransactionRequest <$> aMessage .: "transaction"
            ("Action", "AddToListOfConnects") ->
                ActionAddToListOfConnects <$> aMessage .: "port"

            ("Action", "NodeStillAliveTest") -> do
                aPort        <- aMessage .: "port"
                aIp          <- aMessage .: "ip"
                case readMaybe aIp of
                    Just aJustIp -> return $ ActionNodeStillAliveTest
                        (toEnum aPort) (toHostAddress aJustIp)
                    _ -> mzero
            _ -> mzero


    parseJSON _ = mzero -- error $ show a

readNodeType :: (IsString a, Eq a) => a -> NodeType
readNodeType aNodeType
    | aNodeType == "PoW" = PoW
    | aNodeType == "All" = All
    | otherwise          = PoA


decodeList :: [T.Text] -> [String]
decodeList aList
    | all isRight aDecodeList   = rights aDecodeList
    | otherwise                 = error "Can not decode all transactions in Microblock"
    where aDecodeList = myUnhex <$> aList



instance ToJSON NNToPPMessage where
    toJSON RequestNodeIdToPP = object [
        "tag"   .= ("Request" :: String),
        "type"  .= ("NodeId"  :: String)
      ]

    toJSON (MsgMsgToPP aPPId aMessage) = object [
            "tag"       .= ("Msg"   :: String),
            "type"      .= ("MsgTo" :: String),
            "sender"    .= ppIdToString aPPId,
            "msg"       .= aMessage
          ]

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
        "transaction" .= aTransaction
      ]

    toJSON (MsgBroadcastMsg aMessage (IdFrom aPPId)) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("Broadcast"  :: String),
        "msg"       .= aMessage,
        "idFrom"    .= ppIdToString aPPId
      ]

    toJSON (ResponsePoWList aPPIds) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("PoWList"   :: String),
        "poWList"   .=  map ppIdToString aPPIds
      ]
    toJSON (ResponsePendingTransactions aTransactions) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("Pending"   :: String),
        "transactions" .= aTransactions
      ]
    toJSON (ResponseIsInPending aBool) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("Pending"   :: String),
        "msg"       .= show aBool
      ]
    toJSON (ResponseTransactionValid aBool) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("PendingAdd"   :: String),
        "msg"       .= show aBool
       ]

instance ToJSON Connect where
    toJSON (Connect aHostAddress aPortNumber) = object [
        "ip"   .= show (fromHostAddress aHostAddress),
        "port" .= fromEnum aPortNumber
      ]


--------------------------------------------------------------------------------

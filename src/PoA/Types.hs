{-# LANGUAGE
        OverloadedStrings
    ,   ScopedTypeVariables
    ,   DuplicateRecordFields
    ,   FlexibleInstances
    ,   DeriveGeneric
    ,   GeneralizedNewtypeDeriving
    ,   StandaloneDeriving
  #-}
{-# LANGUAGE GADTs #-}
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
import              Data.Either
import              Text.Read
import              Crypto.PubKey.ECC.ECDSA

data NetMessage where
    RequestTransaction          :: Int                      ->  NetMessage
    RequestPoWList              ::                              NetMessage
    RequestPotentialConnects    :: Bool                     ->  NetMessage
    RequestPending              :: Maybe Transaction        ->  NetMessage

    RequestActualConnects       ::                              NetMessage

    ResponseNodeId              :: NodeId                   ->  NetMessage
    ResponsePotentialConnects   :: [Connect]                ->  NetMessage
    ResponseActualConnects      :: [ActualConnectInfo]      ->  NetMessage
    ResponseTransactions        :: [Transaction]            ->  NetMessage
    ResponseTransactionIsInPending  :: Bool                 ->  NetMessage
    ResponsePoWList             :: [ActualConnectInfo]      ->  NetMessage


    MsgBroadcast                :: IdFrom -> NodeType -> Value ->  NetMessage
    MsgMsgTo                    :: IdFrom -> IdTo     -> Value ->  NetMessage

    MsgMicroblock               :: Microblock   -> NetMessage
    MsgTransaction              :: Transaction  -> NetMessage
    MsgKeyBlock                 :: Value        -> NetMessage

    MsgNewNode                  :: NodeId -> NodeType -> Maybe Connect -> NetMessage

    ActionConnect               :: NodeType   -> Maybe NodeId -> NetMessage
    ActionAddToConnectList      :: PortNumber -> NetMessage
    ActionConnectIsDead         :: Connect    -> NetMessage
  deriving (Show)

data NodeType = PoW | PoA | All | NN deriving (Eq, Show, Ord, Generic)




data ActualConnectInfo = ActualConnectInfo NodeId NodeType (Maybe Connect) deriving Show


instance ToJSON ActualConnectInfo where
    toJSON (ActualConnectInfo aNodeId aNodeType (Just (Connect aIp aPortNumber))) = object [
            "node_type" .= show aNodeType
        ,   "node_id"   .= nodeIdToUnxed aNodeId
        ,   "ip"        .= show (fromHostAddress aIp)
        ,   "port"      .= fromEnum aPortNumber
      ]
    toJSON (ActualConnectInfo aNodeId aNodeType Nothing) = object [
            "node_type" .= show aNodeType
        ,   "node_id"   .= nodeIdToUnxed aNodeId
      ]


instance FromJSON ActualConnectInfo where
    parseJSON (Object aMsg) = do
        aNodeType :: T.Text  <- aMsg .: "node_type"
        aUnxedId  :: T.Text  <- aMsg .: "node_id"
        aNodeId   <- unhexNodeId aUnxedId

        aIp       <- aMsg .:? "ip"
        aPort     <- aMsg .:? "port"

        let aConnect = do
                aIpAdress <- readMaybe =<< aIp
                aJustPort <- aPort
                return $ Connect (toHostAddress aIpAdress) (toEnum aJustPort)
        return $ ActualConnectInfo aNodeId (readNodeType aNodeType) aConnect


unhexNodeId :: MonadPlus m => T.Text -> m NodeId
unhexNodeId aString = case unhex . fromString . T.unpack $ aString of
    Just aDecodeString  -> return . NodeId . roll $ B.unpack aDecodeString
    Nothing             -> mzero

nodeIdToUnxed :: NodeId -> String
nodeIdToUnxed (NodeId aPoint) = CB.unpack . hex . B.pack $ unroll aPoint


myTextUnhex :: T.Text -> Maybe B.ByteString
myTextUnhex aString = fromString <$> aUnxeded
    where
        aUnxeded :: Maybe String
        aUnxeded = unhex aNewString

        aNewString :: String
        aNewString = T.unpack aString


instance FromJSON NetMessage where
    parseJSON (Object aMessage) = do
        aTag  :: T.Text <- aMessage .: "tag"
        aType :: T.Text <- aMessage .: "type"
        --error $ show aTag ++ " " ++ show aType
        case (T.unpack aTag, T.unpack aType) of
            ("Request", "Transaction") -> RequestTransaction <$> aMessage .: "number"

            ("Request","PotentialConnects")    -> do
                aFull :: Maybe T.Text <- aMessage .:? "full"
                return $ RequestPotentialConnects (isJust aFull)

            ("Request","PoWList")     -> return RequestPoWList
            ("Request","ActualConnectList") -> return RequestActualConnects

            ("Request", "Pending") ->
                RequestPending <$> aMessage .:? "transaction"

            ("Msg", "Broadcast") -> do
                aMsg :: Value <- aMessage .: "msg"
                aFrom :: T.Text <- aMessage .: "from"
                aNodeType :: T.Text <-  aMessage .: "node_type"
                aIdFrom   <- unhexNodeId aFrom
                return $ MsgBroadcast (IdFrom aIdFrom) (readNodeType aNodeType) aMsg

            ("Msg", "MsgTo") -> do
                aFrom :: T.Text <- aMessage .: "from"
                aTo   :: T.Text <- aMessage .: "to"
                aMsg  :: Value  <- aMessage .: "msg"
                aIdFrom   <- unhexNodeId aFrom
                aIdTo     <- unhexNodeId aTo
                return $ MsgMsgTo (IdFrom aIdFrom) (IdTo aIdTo) aMsg

            ("Msg", "Microblock") ->
                MsgMicroblock <$> aMessage .: "microblock"

            ("Msg", "Transaction") ->
                MsgTransaction <$> aMessage .: "transaction"

            ("Msg", "KeyBlock") -> MsgKeyBlock <$> aMessage .: "keyBlock"

            ("Msg", "NewNode") -> do
                aId :: T.Text <- aMessage .: "node_id"
                aNodeType :: T.Text <-  aMessage .: "node_type"
                aNodeId   <- unhexNodeId aId
                aConnect  <- aMessage .: "connect"
                return $ MsgNewNode aNodeId (readNodeType aNodeType) aConnect

            ("Response", "NodeId") -> do
                aPPId :: T.Text <- aMessage .: "node_id"

                aNodeId  <- unhexNodeId aPPId
                return $ ResponseNodeId aNodeId

            ("Response", "PotentialConnects") ->
                ResponsePotentialConnects <$> aMessage .: "connects"
            ("Response", "ActualConnects") ->
                ResponseActualConnects <$> aMessage .: "connects"
            ("Response", "Transactions") ->
                ResponseTransactions <$> aMessage .: "transactions"
            ("Response", "TransactionIsInPending") ->
                ResponseTransactionIsInPending <$> aMessage .: "bool"
            ("Response", "PoWList") ->
                ResponsePoWList <$> aMessage .: "connects"

            ("Action", "AddToConnectList") ->
                ActionAddToConnectList . toEnum <$> aMessage .: "port"

            ("Action", "ConnectIsDead") -> ActionConnectIsDead <$> aMessage .: "connect"

            ("Action", "Connect") -> do
                aNodeType :: T.Text  <- aMessage .: "node_type"
                aId                  <- aMessage .:? "node_id"
                case aId of
                    Just aNodeId -> do
                        aJustId <- unhexNodeId aNodeId
                        return $ ActionConnect (readNodeType aNodeType) (Just aJustId)
                    Nothing -> return $ ActionConnect (readNodeType aNodeType) Nothing

            _ -> mzero


    parseJSON _ = mzero -- error $ show a

readNodeType :: (IsString a, Eq a) => a -> NodeType
readNodeType aNodeType
    | aNodeType == "PoW" = PoW
    | aNodeType == "PoA" = PoA
    | aNodeType == "NN"  = NN
    | otherwise          = All


instance ToJSON NetMessage where

    toJSON (ResponsePoWList aConnects) = object [
        "tag"       .= ("Response"   :: String),
        "type"      .= ("PoWList" :: String),
        "connects"  .= aConnects
      ]

    toJSON (MsgMsgTo (IdFrom aIdFrom) (IdTo aIdTo) aMessage) = object [
        "tag"       .= ("Msg"   :: String),
        "type"      .= ("MsgTo" :: String),
        "from"      .= nodeIdToUnxed aIdFrom,
        "to"        .= nodeIdToUnxed aIdTo,
        "msg"       .= aMessage
      ]

    toJSON (MsgBroadcast (IdFrom aIdFrom ) aNodeType aMessage) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("Broadcast"  :: String),
        "msg"       .= aMessage,
        "from"      .= nodeIdToUnxed aIdFrom,
        "node_type" .= show aNodeType
      ]

    toJSON (ResponseNodeId aNodeId) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("NodeId"   :: String),
        "node_id"   .= nodeIdToUnxed aNodeId
      ]

    toJSON (ResponsePotentialConnects aConnects) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("PotentialConnects"  :: String),
        "connects"  .= aConnects
      ]

    toJSON (ResponseActualConnects aConnects) = object [
        "tag"        .= ("Response"  :: String),
        "type"       .= ("ActualConnects"   :: String),
        "connects"   .= aConnects
      ]

    toJSON (ResponseTransactions aTransactions) = object [
        "tag"       .= ("Response"     :: String),
        "type"      .= ("Transaction"  :: String),
        "transactions" .= aTransactions
      ]

    toJSON (ResponseTransactionIsInPending aBool) = object [
        "tag"       .= ("Response"  :: String),
        "type"      .= ("Pending"   :: String),
        "msg"       .= show aBool
       ]

    toJSON (MsgNewNode aPPId aNodeType Nothing) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("NewNodeInNet"  :: String),
        "node_id"    .= nodeIdToUnxed aPPId,
        "node_type"  .= show aNodeType
      ]

    toJSON (MsgNewNode aPPId aNodeType (Just aConnect)) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("NewNodeInNet"  :: String),
        "node_id"    .= nodeIdToUnxed aPPId,
        "node_type"  .= show aNodeType,
        "connect"   .= aConnect
      ]

    toJSON (MsgMicroblock aMicroblock) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("Microblock"  :: String),
        "microblock" .= aMicroblock
      ]

    toJSON (MsgTransaction aTransaction) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("Transaction"  :: String),
        "transaction" .= aTransaction
      ]


    toJSON (MsgKeyBlock aKeyBlock) = object [
        "tag"       .= ("Msg"           :: String),
        "type"      .= ("KeyBlock"  :: String),
        "keyBlock" .= aKeyBlock
      ]

    toJSON (RequestTransaction aNum) = object [
        "tag"       .= ("Request"      :: String),
        "type"      .= ("Transaction"  :: String),
        "number" .= aNum
      ]

    toJSON RequestPoWList = object [
        "tag"       .= ("Request"      :: String),
        "type"      .= ("PoWList"  :: String)
      ]
    toJSON (RequestPending (Just aTransaction)) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("Pending"  :: String),
        "transaction"   .= aTransaction
      ]
    toJSON (RequestPending Nothing) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("Pending"  :: String)
      ]
    toJSON (RequestPotentialConnects True) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("PotentialConnects"  :: String),
        "full"          .= ("True"  :: String)
      ]
    toJSON (RequestPotentialConnects _) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("PotentialConnects"  :: String)
      ]
    toJSON RequestActualConnects = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("ActualConnectList"  :: String)
      ]

    toJSON (RequestPending (Just aTransaction)) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("Pending"  :: String),
        "transaction"   .= aTransaction
      ]

    toJSON (RequestPending _) = object [
        "tag"           .= ("Request"      :: String),
        "type"          .= ("Pending"  :: String)
      ]

    toJSON (ActionAddToConnectList aPortNumber) = object [
        "tag"  .= ("Action"      :: String),
        "type" .= ("AddToConnectList"  :: String),
        "port" .= fromEnum aPortNumber
      ]

    toJSON (ActionConnect aNodeType (Just aJustId)) = object [
        "tag"  .= ("Action"      :: String),
        "type" .= ("Connect"  :: String),
        "node_type" .= show aNodeType,
        "node_id"   .= nodeIdToUnxed aJustId
      ]

    toJSON (ActionConnect aNodeType _) = object [
        "tag"  .= ("Action"      :: String),
        "type" .= ("Connect"  :: String),
        "node_type" .= show aNodeType
      ]

    toJSON (ActionConnectIsDead aConnect) = object [
        "tag"  .= ("Action"      :: String),
        "type" .= ("ConnectIsDead"  :: String),
        "connect" .= aConnect
      ]

instance ToJSON Connect where
    toJSON (Connect aHostAddress aPortNumber) = object [
        "ip"   .= show (fromHostAddress aHostAddress),
        "port" .= fromEnum aPortNumber
      ]

instance FromJSON Connect where
    parseJSON (Object aConnect) = do
        aIp     <- aConnect .: "ip"
        aPort   <- aConnect .: "port"
        case readMaybe aIp of
            Nothing      -> mzero
            Just aJustIp -> return $
                Connect (toHostAddress aJustIp) (toEnum aPort)

--------------------------------------------------------------------------------

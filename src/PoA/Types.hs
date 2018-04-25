{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import              Service.Types (Microblock(..))

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

    -- Ответы с UUID
    | ResponseUUID {
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
                -- case unhex aSender of
                --     Just aDecodeString -> do
                --         case S.decode aDecodeString of
                --             Right aPoint -> return (RequestMsgTo $ UUID aPoint)
                --             Left  aError -> mzero
                --     Nothing -> mzero

            ("Request", "Transaction") -> RequestTransaction <$> aMessage .: "number"

            ("Request", "Broadcast") -> do
                aMsg :: T.Text <- aMessage .: "msg"
                case myTextUnhex aMsg of
                    Just aUnxededMsg  -> return $ RequestBroadcast aUnxededMsg
                    Nothing           -> mzero

            ("Response", "UUID") -> do
                aUuid :: T.Text <- aMessage .: "uuid"
                aPoint <- myUnhex aUuid
                return (RequestMsgTo $ UUID aPoint)

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

-- Eather a String

decodeList :: S.Serialize a => [T.Text] -> [a]
decodeList aList
    | all isRight aDecodeList   = rights aDecodeList
    | otherwise                 = []
    where aDecodeList = S.decode <$> fromString . T.unpack <$> aList


--------------------------------------------------------------------------------
{-
##### Примечание
Передача байтовых массивов и UUID, строкой с 16-ричной кодировкой.

## Общая часть
### Сообщения от ПоА/ПоВ для НН

#### Запросы
На получение сообщения от PoA/PoW ноды
```
{
    "tag":"Request",
    "type":"MsgTo",
    "sender":"xxxx"
}
```

#### Ответы
С UUID
```
{
    "tag":"Response",
    "type":"UUID",
    "UUID":"xxxx"
}
```
#### Сообщения
Для другой PoA/PoW ноды
```
{
    "tag":"Msg",
    "type":"MsgTo",
    "destination":"xxxx",
    "msg":"xxx"
}
```

### Сообщения для ПоА/ПоВ от НН
#### Запросы

На получение UUID
```
{
    "tag":"Request",
    "type":"UUID"
}
```

#### Ответы
С сообщениями от другой PoA/PoW ноды.

```
{
    "tag":"Response",
    "type":"MsgTo",
    "sender":"xxxx",
    "messages": ["xxx"]
}
```

#### Сообщения
О том, что следует создать соединение с вот той нодой и она обеспечивает лучшую связь.
```
{
    "tag":"Msg",
    "type":"Connect",
    "ip":"xxxx",
    "port": "xxx"
}
```

О том, что появилась новая ПоА/ПоВ нода.
```
{
    "tag":"Msg",
    "type":"NewNodeInNet",
    "id": "xxxx",
    "nodeType" : "PoW" | "PoA"
}
```

## Специфичная часть

### Сообщения от ПоА для НН

#### Запросы
На получение транзакций
```
{
    "tag":"Request",
    "type":"Transaction",
    "number":3
}
```

#### Сообщения

О том, что микроблок намйнился
```
{
    "tag":"Msg",
    "type":"Microblock",
    "microblock": {
        previousHash : "xxxx",
        blockHash: "xxx",
        transactions : [
            "xxxx",
            "xxxx"
        ]
    }
}
```

### Сообщения для ПоА от НН


#### Ответы
С транзакцией
```
{
    "tag":"Response",
    "type": "Transaction",
    "transaction": "xxx"
}

```


### Сообщения от ПоВ к НН
#### Запрос
Запрос на рассылку бродкаста.
```
{
    "tag": "Request",
    "type": "Broadcast",
    "msg": "XXX"
}
```
### Сообщения для ПоВ от НН
#### Ответы
С сообщениями полученные бродкастом.

```
{
    "tag":"Response",
    "type":"BroadcastMsg",
    "messages": ["xxx"]
}
```
-}
--------------------------------------------------------------------------------

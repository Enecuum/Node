module PoA.Types where

import Data.Word
import Data.ByteString

type UUID = (Word64, Word64)

data PoAPoWToNnMessage
    -- Запросы:
    -- на получение сообщения от PoA/PoW ноды.
    = RequestMsgTo {
        sender :: UUID
    }

    -- на получение транзакций.
    | RequestTransaction {
        number :: Int
    }

    -- запрос на рассылку бродкаста.
    | RequestBroadcast {
        msg :: ByteString
    }

    -- Ответы с UUID
    | ResponseUUID {
        uuid :: UUID
    }

    -- Сообщения:
    -- Для другой PoA/PoW ноды.
    | MsgMsgTo {
        destination :: UUID,
        msg :: ByteString
    }

    -- О том, что намйнился микроблок.
    | MsgMicroblock {
        previousHash :: ByteString,
        blockHash    :: ByteString,
        transactions :: [ByteString]
    }

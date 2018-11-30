module Enecuum.Assets.Nodes.ConfigParsing where

import           Data.Yaml                   (ParseException, prettyPrintParseException)
import qualified Enecuum.Assets.Scenarios    as A
import qualified Enecuum.Assets.TstScenarios as Tst
import qualified Enecuum.Config              as Cfg
import           Enecuum.Prelude

-- | runParser return exception (Left ParseException) or just dummy value (1) for convenience
runParser
    :: Show node
    => Show (Cfg.NodeConfig node)
    => Show (Cfg.NodeScenario node)
    => Either ParseException (Cfg.Config node)
    -> IO (Either ParseException Int)
runParser (Left e)  = pure $ Left e
runParser (Right _) = pure $ Right 1

-- Try to parse config of unknown type (parseConfig failure invoke error)
parseConfig :: LByteString -> IO ()
parseConfig configSrc = do
    let runners =
            [ runParser $ Cfg.tryParseConfig @A.GraphNode  configSrc
            , runParser $ Cfg.tryParseConfig @A.PoANode    configSrc
            , runParser $ Cfg.tryParseConfig @A.PoWNode    configSrc
            , runParser $ Cfg.tryParseConfig @A.ClientNode configSrc
            , runParser $ Cfg.tryParseConfig @A.BN         configSrc
            , runParser $ Cfg.tryParseConfig @A.TestClient configSrc
            , runParser $ Cfg.tryParseConfig @A.TestServer configSrc

            , runParser $ Cfg.tryParseConfig @Tst.NN               configSrc
            , runParser $ Cfg.tryParseConfig @Tst.TstGraphNode     configSrc
            , runParser $ Cfg.tryParseConfig @Tst.TstGenPoWNode    configSrc
            , runParser $ Cfg.tryParseConfig @Tst.TstGenPoANode    configSrc
            , runParser $ Cfg.tryParseConfig @Tst.TstRealPoWNode   configSrc
            ]

    results <- sequence runners
    let typeConfigMatch = rights results
    when (length typeConfigMatch == 0) $ do
        let exceptions = map prettyPrintParseException $ lefts results
        let exception = guessAppropriateException exceptions
        error $ show $ exception

-- packFrequency "aaabccaac" == [(5,'a'),(1,'b'),(3,'c')]
packFrequency :: (Eq a, Ord a) => [a] -> [(Int, a)]
packFrequency xs = map (\x -> (length x, head x) ) $ group $ sort xs

-- chooseSingleException [(5,'a'),(1,'b'),(3,'c')] == 'b'
chooseSingleException :: [(Int, a)] -> a
chooseSingleException []     = error "Impossible happend. Config is the same for all types of nodes"
chooseSingleException [x]    = snd x
chooseSingleException (x:xs) = if fst x == 1 then snd x else chooseSingleException xs

-- | Guess exception for config of unknown type (apparently it is the most rare exception)
guessAppropriateException :: (Eq a, Ord a) => [a] -> a
guessAppropriateException exceptions = chooseSingleException $ packFrequency exceptions

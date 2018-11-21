module Enecuum.Tests.Integration.ConfigsSpec where

import qualified Data.ByteString.Lazy               as LBS
import           Data.Typeable
import           Data.Yaml
import qualified Enecuum.Assets.Scenarios           as A (BN, ClientNode, GraphNode, NN, PoANode, PoWNode, TestClient,
                                                          TestServer)
import           Enecuum.Assets.System.Directory    (configDir)
import           Enecuum.Config
import qualified Enecuum.Config                     as Cfg
import qualified Enecuum.Framework.Node.Interpreter as I
import qualified Enecuum.Language                   as L
import           Enecuum.Prelude
import           Enecuum.Tests.Wrappers
import           System.Directory
import           System.FilePath                    ((</>))
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit           (fromHUnitTest)
import           Test.HUnit

instance Ord ParseException where
    (AesonException s1 ) `compare` (AesonException s2 ) = s1 `compare` s2
instance Eq ParseException where
    (AesonException s1) == (AesonException s2) = s1 == s2

spec :: Spec
spec = fastTest $ describe "Validate configs" $ do
    configFiles <- runIO $ listDirectory configDir
    fromHUnitTest $ TestList $ map (\file -> TestLabel ("Parse config " +|| file ||+ "") (parse file)) configFiles

-- | Try to parse config of unknown type
parse :: FilePath -> Test
parse file = TestCase $ do
    let filename = configDir </> file
    configSrc <- LBS.toStrict <$> (I.runNodeL undefined $ L.readFile filename)
    let runners =
          [ runParser $ Cfg.tryParseConfig @A.GraphNode  configSrc
          , runParser $ Cfg.tryParseConfig @A.PoANode    configSrc
          , runParser $ Cfg.tryParseConfig @A.PoWNode    configSrc
          , runParser $ Cfg.tryParseConfig @A.ClientNode configSrc
          , runParser $ Cfg.tryParseConfig @A.NN         configSrc
          , runParser $ Cfg.tryParseConfig @A.BN         configSrc
          , runParser $ Cfg.tryParseConfig @A.TestClient configSrc
          , runParser $ Cfg.tryParseConfig @A.TestServer configSrc
          ]

    results <- sequence runners
    let typeConfigMatch = rights results
    when (length typeConfigMatch == 0) $ do
        let exceptions = lefts results
        let exception = guessAppropriateException exceptions
        error $ show $ prettyPrintParseException exception
    (length typeConfigMatch) `shouldNotBe` 0

-- | runParser return exception (Left ParseException) or just dummy value (1) for convenience
runParser
    :: Show node
    => Show (Cfg.NodeConfig node)
    => Show (Cfg.NodeScenario node)
    => Either ParseException (Config node)
    -> IO (Either ParseException Int)
runParser (Left e)    = pure $ Left e
runParser (Right cfg) = pure $ Right 1

-- packFrequency "aaabccaac" == [(5,'a'),(1,'b'),(3,'c')]
packFrequency :: (Eq a, Ord a) => [a] -> [(Int, a)]
packFrequency xs = map (\x -> (length x, head x) ) $ group $ sort xs

-- chooseSingleException [(5,'a'),(1,'b'),(3,'c')] == 'b'
chooseSingleException :: [(Int, a)] -> a
chooseSingleException (x:xs) = if fst x == 1 then snd x else chooseSingleException xs

-- | Guess exception for config of unknown type (apparently it is the most rare exception)
guessAppropriateException :: (Eq a, Ord a) => [a] -> a
guessAppropriateException exceptions = chooseSingleException $ packFrequency exceptions

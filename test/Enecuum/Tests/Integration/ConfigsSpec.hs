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

spec :: Spec
spec = fastTest $ describe "Test config validity" $ fromHUnitTest $ TestList
        -- []
        [TestLabel "Parse configs" testParseConfigs]

testParseConfigs :: Test
testParseConfigs = TestCase $ do
    configFiles <- listDirectory configDir
    void $ sequence $ map parse configFiles

parse :: FilePath -> IO ()
parse file = do
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
    let typeConfigMatch = catMaybes results
    when (length typeConfigMatch == 0) $ do
        print filename
    (length typeConfigMatch) `shouldNotBe` 0

runParser
    :: Show node
    => Show (Cfg.NodeConfig node)
    => Show (Cfg.NodeScenario node)
    => Either ParseException (Config node)
    -> IO (Maybe ())
-- runParser = undefined
runParser (Left e) = pure Nothing
runParser (Right cfg) = do
    putStrLn @Text $
        "Parse config..." <>
        "\n    Node:     " +|| Cfg.node cfg         ||+
        "\n    Scenario: " +|| Cfg.nodeScenario cfg ||+ ""
    pure $ Just ()

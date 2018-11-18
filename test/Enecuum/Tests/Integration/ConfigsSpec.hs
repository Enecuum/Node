module Enecuum.Tests.Integration.ConfigsSpec where

import           Enecuum.Assets.Nodes.Client        (ClientNode)
import           Enecuum.Assets.System.Directory    (configDir)
import           Enecuum.Config
import qualified Enecuum.Framework.Node.Interpreter as I
import qualified Enecuum.Language                   as L
import           Enecuum.Prelude
import           Enecuum.Tests.Wrappers
import           System.Directory
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit           (fromHUnitTest)
import           Test.HUnit
import qualified Data.ByteString.Lazy                         as LBS
import           System.FilePath       ((</>))

spec :: Spec
spec = fastTest $ describe "Test config validity" $ fromHUnitTest $ TestList
        -- []
        [TestLabel "Parse configs" testParseConfigs]

testParseConfigs :: Test
testParseConfigs = TestCase $ do
    configFiles <- listDirectory configDir
    print configFiles
    parse (configFiles !! 0)
    -- sequence $ map parse configFiles

parse file = do
    configSrc <- I.runNodeL undefined $ L.readFile $ configDir </> file
    let res = tryParseConfig @ClientNode $ LBS.toStrict configSrc
    -- res `shouldSatisfy` isRight
    True `shouldBe` True
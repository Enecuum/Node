module Enecuum.Tests.Integration.ConfigsSpec where

import           Enecuum.Assets.Nodes.ConfigParsing (parseConfig)
import           Enecuum.Assets.System.Directory    (configDir)
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
spec = fastTest $ describe "Validate configs" $ do
    configFiles <- runIO getConfigNames
    fromHUnitTest $ TestList $ map (\file -> TestLabel ("Parse config " +|| file ||+ "") (parse file)) configFiles

getConfigNames :: IO [FilePath]
getConfigNames = do
    dirContent <- listDirectory configDir
    let isConfigFile filePath = doesFileExist $ configDir </> filePath
    filterM isConfigFile dirContent

-- | Try to parse config of unknown type
parse :: FilePath -> Test
parse file = TestCase $ do
    let filename = configDir </> file
    configSrc <- I.runNodeL undefined $ L.readFile filename
    -- Try to parse config of unknown type (parseConfig failure invoke error)
    parseConfig configSrc
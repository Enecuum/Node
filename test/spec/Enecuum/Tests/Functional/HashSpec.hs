module Enecuum.Tests.Functional.HashSpec where

import           Enecuum.Prelude

import           Data.HGraph.StringHashable
import           Enecuum.Domain
import           Enecuum.Testing.Wrappers
import           Test.Hspec

import qualified Enecuum.Samples.Blockchain.Language                        as L
import           Enecuum.Samples.Blockchain.Domain                          
spec :: Spec
spec = fastTest $ describe "Nodes test" $
    it "Hash of genesis block is" $
        toHash genesisKBlock `shouldBe` StringHash "4z9ADFAWehl6XGW2/N+2keOgNR921st3oPSVxv08hTY="

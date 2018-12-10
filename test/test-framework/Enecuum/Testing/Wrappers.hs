module Enecuum.Testing.Wrappers where

import           Test.Hspec (Spec, SpecWith, describe)

fastTest :: SpecWith () -> Spec
fastTest = describe "Fast"

slowTest :: SpecWith () -> Spec
slowTest = describe "Slow"

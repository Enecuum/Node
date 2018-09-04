module Enecuum.Legacy.Service.System.Version where

import           Language.Haskell.TH
import           System.Process
import           Universum

version :: Q Exp
version = do
    aVersion <- runIO $ readProcess "git" ["rev-parse", "HEAD"] []
    litE (StringL $ take 7 aVersion)

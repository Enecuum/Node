{-# LANGUAGE ScopedTypeVariables #-}
module Enecuum.Legacy.Node.Node.Config.Make where

import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as L
import           Enecuum.Legacy.Node.Node.Types
import           System.Directory         (createDirectoryIfMissing)


makeFileConfig :: IO ()
makeFileConfig = do
    nConfig <- makeNewNodeConfig
    createDirectoryIfMissing True "configs"
    L.writeFile "configs/nodeInfo.json" $ encodePretty nConfig

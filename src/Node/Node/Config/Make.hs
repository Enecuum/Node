{-# LANGUAGE ScopedTypeVariables #-}
module Node.Node.Config.Make where

import              Node.Node.Types
import qualified    Data.ByteString.Lazy as L
import              System.Directory (createDirectoryIfMissing)
import              Data.Aeson.Encode.Pretty

makeFileConfig :: IO ()
makeFileConfig = do
    nConfig <- makeNewNodeConfig
    createDirectoryIfMissing True "configs"
    L.writeFile "configs/nodeInfo.json" $ encodePretty nConfig

{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import Service.Network.WebSockets.Client
import Network.WebSockets hiding (runClient)
import Data.ByteString

main :: IO ()
main = runClient "95.216.150.208" 1554 "/" $ \aConnect -> sendBinaryData aConnect ("Binary" :: ByteString)

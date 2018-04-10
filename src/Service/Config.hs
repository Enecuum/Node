{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Service.Config 
     (
       getConfigValue
     ) where

import Data.Ini
import Control.Exception (SomeException(), try)
import System.Environment (getEnv)
import Data.Text
import Prelude hiding (concat)

getConfigValue :: Ini -> Text -> Text -> IO String
getConfigValue ini group name = do
   try (getEnv $ unpack $ concat [group,name]) >>= \case
       Right item              -> return $ item
       Left (_::SomeException) -> case lookupValue group name ini of
            Right item  -> return $ unpack item
            Left e      -> error    e


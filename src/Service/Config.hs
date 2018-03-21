module Service.Config 
     (
       getVar
     , defaultConfig
     , findConfigFile
     ) where

import Data.Ini
import Data.Text (pack, unpack)
import System.Directory


defaultConfig :: FilePath
defaultConfig = "configs/config.ini"

getIni :: FilePath -> IO (Maybe Ini)
getIni path = do
        result <- readIniFile path
        case result of
          Left err  -> do
               putStrLn err
               return Nothing
          Right ini -> return $ Just ini

getVar :: FilePath -> String -> String -> IO (Maybe String)
getVar path section key = do
        maybeIni <- getIni path
        case maybeIni of
          Nothing  -> return Nothing
          Just ini -> do
               case lookupValue (pack section) (pack key) ini of
                 Left err    -> do
                      putStrLn err
                      return Nothing
                 Right value -> return $ Just $ unpack value 


findConfigFile :: [String] -> IO (Maybe FilePath)
findConfigFile args = if length args > 0
                      then return $ Just $ head args
                      else do
                        ex <- doesFileExist defaultConfig
                        if ex 
                        then return $ Just defaultConfig
                        else do
                          putStrLn "Please, specify config file"
                          return Nothing
                         

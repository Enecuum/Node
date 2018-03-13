{-#LANGUAGE ScopedTypeVariables#-}

module Node.FileDB.FileDB (
    addDataToFile,
    readDataFile,
    readHashMsgFromFile--,
    --deleteDataFromFile
  ) where

import Control.Exception

import qualified Data.ByteString as B
import Service.Types
import Data.Serialize as S
import System.Directory



readDataFile :: Read a => FilePath -> IO [a]
readDataFile aFilePath = do
    aFileContent <- try $ unsafeReadDataFile aFilePath
    case aFileContent of
        Right aJustFileContent    -> pure aJustFileContent
        Left (_ :: SomeException) -> do
            aOk <- try $ writeFile aFilePath ""
            case aOk of
                Right _                   -> return ()
                Left (_ :: SomeException) -> do
                    createDirectory "./data"
                    writeFile aFilePath ""
            pure []


unsafeReadDataFile :: Read a => FilePath -> IO [a]
unsafeReadDataFile aFilePath = do
    aFileContent <- readFile aFilePath
    return $ read <$> lines aFileContent


{-
deleteDataFromFile :: (Show a, Read a, Eq a) => FilePath -> a -> IO ()
deleteDataFromFile aFilePath aElem = do
    aFile <- try $ unsafeReadDataFile aFilePath
    case aFile of
        Right aElems -> writeDataToFile aFilePath $ delete aElem aElems
        Left (_ :: SomeException) -> pure ()

-}

addDataToFile :: Show a => FilePath -> a -> IO ()
addDataToFile aFilePath aData = do
    aOk1 <- try $ appendFile aFilePath $ show aData ++ "\n"
    case aOk1 of
        Right _ -> pure ()
        Left _ -> do
            aOk2 <- try $ writeFile aFilePath $ show aData ++ "\n"
            case aOk2 of
                Right _                   -> pure ()
                Left (_ :: SomeException) -> do
                    createDirectory "./data"
                    writeFile aFilePath $ show aData ++ "\n"


writeDataToFile :: Show a => FilePath -> [a] -> IO ()
writeDataToFile aFilePath aElems = do
    writeFile aFilePath . unlines $ show <$> aElems

readHashMsgFromFile :: String -> IO [Microblock]
readHashMsgFromFile filename = do
    aFileContent <- B.readFile filename
    case S.decode aFileContent of
        Right aMicroblocks  -> return aMicroblocks
        Left aError         -> do
            putStrLn $ "error " ++ show aError
            return []

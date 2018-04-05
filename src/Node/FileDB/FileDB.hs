{-#LANGUAGE ScopedTypeVariables#-}

module Node.FileDB.FileDB (
        addDataToFile
    ,   readDataFile
    ,   readHashMsgFromFile
    ,   writeDataToFile
) where

import Control.Exception
import Control.Monad

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

writeDataToFile :: Show a => FilePath -> [a] -> IO ()
writeDataToFile aFilePath aDataLis = do
    let aWriteFile = writeFile aFilePath $ concat
            [show aData ++ "\n" | aData <- aDataLis]
    aOk1 <- try aWriteFile
    case aOk1 of
        Right _ -> pure ()
        Left (_ :: SomeException) -> do
            aOk2 <- try aWriteFile
            case aOk2 of
                Right _                   -> pure ()
                Left (_ :: SomeException) -> do
                    createDirectory "./data"
                    aWriteFile


addDataToFile :: Show a => FilePath -> [a] -> IO ()
addDataToFile aFilePath aDataList = do
    aOk1 <- try $ forM_ aDataList $ \aData ->
        appendFile aFilePath $ show aData ++ "\n"
    case aOk1 of
        Right _ -> pure ()
        Left (_ :: SomeException) -> writeDataToFile aFilePath aDataList

{-
writeDataToFile :: Show a => FilePath -> [a] -> IO ()
writeDataToFile aFilePath aElems = do
    writeFile aFilePath . unlines $ show <$> aElems
-}

readHashMsgFromFile :: String -> IO [Microblock]
readHashMsgFromFile filename = do
    result <- try $ B.readFile filename
    case result of
        Right aFileContent          -> case S.decode aFileContent of
            Right aMicroblocks  -> return aMicroblocks
            Left aError         -> do
                putStrLn $ "error " ++ show aError
                return []
        Left  ( _ :: SomeException) -> do
            putStrLn $ filename ++ "does not exist"
            return []

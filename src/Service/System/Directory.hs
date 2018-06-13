module Service.System.Directory (
    getTime,
    getKeyFilePath,
    getTransactionFilePath,
    createFilesDirectory,
    getLedgerFilePath
  )where

import System.FilePath.Posix (takeDirectory)
import System.Directory      (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath       (pathSeparator)
import Data.Time.Clock       (getCurrentTime, utctDayTime)


getTime :: IO Double
getTime = fromRational . toRational . utctDayTime <$> getCurrentTime

getEnecuumDir :: IO String
getEnecuumDir = do
    homeDir <- getHomeDirectory
    let enecuumDir = homeDir ++ [pathSeparator] ++ "enecuum"
    createDirectoryIfMissing True enecuumDir
    return enecuumDir


getKeyFilePath :: IO String
getKeyFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "key.txt")


getTransactionFilePath :: IO String
getTransactionFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "tx")

getLedgerFilePath :: IO String
getLedgerFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "ledger.db")

createFilesDirectory :: FilePath -> IO ()
createFilesDirectory path = createDirectoryIfMissing True $ takeDirectory path

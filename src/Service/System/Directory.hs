module Service.System.Directory (
    getTime,
    getKeyFilePath,
    createFilesDirectory,
    getTransactionFilePath,
    getLedgerFilePath,
    getMicroblockFilePath,
    getMacroblockFilePath,
    getSproutFilePath,
    getLastFilePath
  )where

import           Data.UnixTime
import           System.Directory      (createDirectoryIfMissing,
                                        getHomeDirectory)
import           System.FilePath       (pathSeparator)
import           System.FilePath.Posix (takeDirectory)


getTime :: IO Int
getTime = fromEnum <$> utSeconds <$> getUnixTime

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
    return (enecuumDir ++ [pathSeparator] ++ "tx.db")

getLedgerFilePath :: IO String
getLedgerFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "ledger.db")

getMicroblockFilePath :: IO String
getMicroblockFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "microblock.db")

getMacroblockFilePath :: IO String
getMacroblockFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "macroblock.db")

getSproutFilePath :: IO String
getSproutFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "sprout.db")

getLastFilePath :: IO String
getLastFilePath = do
    enecuumDir <- getEnecuumDir
    return (enecuumDir ++ [pathSeparator] ++ "last.db")

createFilesDirectory :: FilePath -> IO ()
createFilesDirectory path = createDirectoryIfMissing True $ takeDirectory path

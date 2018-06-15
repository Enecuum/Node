{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import              Control.Monad
import              Control.Concurrent
import              System.Environment (getEnv)
import              Node.Data.Key

import              Data.Maybe (fromJust)
import              Node.Node.Mining
import              Node.Node.Types
import              Node.Data.Key (generateKeyPair)
import              Service.Types.PublicPrivateKeyPair (fromPublicKey256k1, compressPublicKey)
import              Service.Timer
import              Node.Lib
import              Service.InfoMsg
import              Service.Network.Base
import              PoA.PoAServer
import              CLI.CLI
import              CLI.RPC
import              Control.Exception (try, SomeException())
import              Data.IP

import              Data.Aeson (decode)
import              Data.Aeson.Encode.Pretty (encodePretty)
import qualified    Data.ByteString.Lazy as L
import              Service.Transaction.Storage (startDB) --(DBdescriptor(..), startDB)


configName :: String
configName = "configs/config.json"

main :: IO ()
main =  do
        putStrLn "testNet 15/06/2017 08:40"
        enc <- L.readFile configName
        case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            aExitCh   <- newChan
            aAnswerCh <- newChan
            aInfoCh   <- newChan
            descrDB   <- startDB

            void $ startNode descrDB conf
                aExitCh aAnswerCh aInfoCh managerMining $ \ch aChan aMyNodeId aFileChan -> do
                    -- periodically check current state compare to the whole network state
                    metronomeS 400000 (writeChan ch connectivityQuery)
                    metronomeS 1000000 (writeChan ch queryPositions)
                    metronomeLinear 100000 100000000 (writeChan ch infoRequest)
                    metronomeS 1000000 (writeChan ch deleteOldestMsg)
                    metronomeS 1000000 (writeChan ch deleteOldestPoW)
                    metronomeS 1000000 (writeChan ch findBestConnects)

                    snbc    <- try (pure $ fromJust $ simpleNodeBuildConfig conf) >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> error "Please, specify simpleNodeBuildConfig"

                    poa_p   <- try (getEnv "poaPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> return $ poaPort conf

                    stat_h  <- try (getEnv "statsdHost") >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> return $ host $ statsdBuildConfig conf

                    stat_p  <- try (getEnv "statsdPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> return $ port $ statsdBuildConfig conf

                    logs_h  <- try (getEnv "logHost") >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> return $ host $ logsBuildConfig conf

                    logs_p  <- try (getEnv "logPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> return $ port $ logsBuildConfig conf

                    log_id  <- try (getEnv "logId") >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> return $ show aMyNodeId

                    test_send <- try (getEnv "test_send_id") >>= \case
                        Right idTo              -> (metronomeS 10000000 (writeChan ch (testSendMessage ((read idTo) :: NodeId))))
                        Left (e::SomeException) -> print e
                    print test_send

                    i_am_firs <- try (getEnv "isFirst") >>= \case
                        Right "Yes" -> return True
                        Right _   -> return False
                        Left (_::SomeException) -> return False

                    when i_am_firs $ writeChan ch InitShardingLvl

                    void $ forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoCh log_id


                    void $ forkIO $ servePoA poa_p aMyNodeId ch aChan aInfoCh aFileChan aMicroblockChan

                    cli_m   <- try (getEnv "cliMode") >>= \case
                            Right item              -> return item
                            Left (_::SomeException) -> return $ cliMode snbc

                    void $ forkIO $ case cli_m of
                      "rpc" -> do
                            rpcbc <- try (pure $ fromJust $ rpcBuildConfig snbc) >>= \case
                                       Right item              -> return item
                                       Left (_::SomeException) -> error "Please, specify RPCBuildConfig"

                            rpc_p <- try (getEnv "rpcPort") >>= \case
                                  Right item              -> return $ read item
                                  Left (_::SomeException) -> return $ rpcPort rpcbc

                            ip_en <- join $ enableIPsList <$> (try (getEnv "enableIP") >>= \case
                                  Right item              -> return $ read item
                                  Left (_::SomeException) -> return $ enableIP rpcbc)

                            token <- try (getEnv "token") >>= \case
                                  Right item              -> return $ read item
                                  Left (_::SomeException) -> case accessToken rpcbc of
                                       Just token -> return token
                                       Nothing    -> updateConfigWithToken conf snbc rpcbc

                            serveRpc descrDB rpc_p ip_en ch aInfoCh


                      "cli" -> serveCLI descrDB ch aInfoCh

                      _     -> return ()



                    --when (takeEnd 3 log_id == "175") $
                    metronomeS 10000000 (writeChan ch testBroadcastBlockIndex)


                    writeChan aInfoCh $ Metric $ increment "cl.node.count"

            void $ readChan aExitCh



updateConfigWithToken :: BuildConfig -> SimpleNodeBuildConfig -> RPCBuildConfig -> IO Token
updateConfigWithToken conf snbc rpcbc = do
      token <- fromPublicKey256k1 <$> compressPublicKey <$> fst <$> generateKeyPair
      let newConfig = conf { simpleNodeBuildConfig = Just $
                               snbc  { rpcBuildConfig = Just $
                                 rpcbc { accessToken = Just token }
                                     }
                           }

      putStrLn $ "Access available with token: " ++ show token

      L.writeFile configName $ encodePretty newConfig

      return token

enableIPsList :: [String] -> IO [AddrRange IPv6]
enableIPsList []  = return [ read "::/0" ]
enableIPsList ips = sequence $ map (\ip_s -> try (readIO ip_s :: IO IPRange) >>= \case
                            Right (IPv4Range r) -> if r == read "0.0.0.0"
                                                   then return $ read "::/0"
                                                   else return $ ipv4RangeToIPv6 r
                            Right (IPv6Range r) -> if r == read "::"
                                                   then return $ read "::/0"
                                                   else return r
                            Left (_ :: SomeException) -> error $ "Wrong IP format"
                            )
                               ips

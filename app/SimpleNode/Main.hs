{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import              Control.Monad
import              Control.Concurrent
import              System.Environment (getEnv)
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

configName :: String
configName = "configs/config.json"

main :: IO ()
main =  do
        enc <- L.readFile configName
        case decode enc :: Maybe BuildConfig of
          Nothing   -> error "Please, specify config file correctly"
          Just conf -> do

            aExitCh   <- newChan
            aAnswerCh <- newChan
            aInfoCh   <- newChan

            void $ startNode conf
                aExitCh aAnswerCh aInfoCh managerMining $ \ch aChan aMyNodeId aFileChan -> do
                    -- periodically check current state compare to the whole network state
                    metronomeS 400000 (writeChan ch connectivityQuery)
                    metronomeS 1000000 (writeChan ch deleteOldestMsg)
                    metronomeS 1000000 (writeChan ch deleteOldestPoW)

                    snbc    <- try (pure $ fromJust $ simpleNodeBuildConfig conf) >>= \case 
                            Right item              -> return item
                            Left (_::SomeException) -> error "Please, specify SimpleNodeBuildConfig" 

                    poa_in  <- try (getEnv "poaInPort") >>= \case
                            Right item              -> return $ read item
                            Left (_::SomeException) -> return $ poaInPort snbc

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

                    void $ forkIO $ serveInfoMsg (ConnectInfo stat_h stat_p) (ConnectInfo logs_h logs_p) aInfoCh log_id

                    void $ forkIO $ servePoA poa_in aMyNodeId ch aChan aInfoCh aFileChan

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

                            serveRpc rpc_p ip_en ch aInfoCh


                      "cli" -> serveCLI ch aInfoCh

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

enableIPsList :: [String] -> IO [IPRange]
enableIPsList ips = sequence $ map (\ip_s -> try (readIO ip_s :: IO IPRange) >>= \case
                            Right range_ip            -> return range_ip
                            Left (_ :: SomeException) -> try (readIO ip_s :: IO IP) >>= \case
                                 Right (IPv4 ipv4)          -> return $ IPv4Range $ makeAddrRange ipv4 32
                                 Right (IPv6 ipv6)          -> return $ IPv6Range $ makeAddrRange ipv6 128
                                 Left  (_ :: SomeException) -> error $ "Wrong IP format"
                            )
                               ips

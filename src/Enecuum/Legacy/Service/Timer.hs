module Enecuum.Legacy.Service.Timer where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Universum

data MetronomeComand = StopMetronome


timer :: Int -> IO () -> IO ()
timer x aFunc = void $ forkIO $ do
    threadDelay x
    aFunc

metronomeLinear :: Int -> Int -> IO () -> IO ()
metronomeLinear aMinT aMaxT aFunc = void $ forkIO $ aLoop 0
  where
    aLoop x = do
        let aTime = if aMinT * x < aMaxT then aMinT * x else aMaxT
        threadDelay aTime
        aFunc
        aLoop (x+1)


metronome :: Int -> IO () -> IO ()
metronome x aFunc = void $ forkIO $ forever $ do
    threadDelay x
    aFunc


metronomeS :: Int -> IO () -> IO ()
metronomeS x aFunc = void $ forkIO $ forever $ do
    aFunc
    threadDelay x


stopebleMetronome :: Int -> IO () -> IO (Chan MetronomeComand)
stopebleMetronome x aFunc = do
    chan <- newChan
    void . forkIO . void $ race (metronome x aFunc) (void $ readChan chan)
    return chan

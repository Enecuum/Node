module Service.Timer where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async

data MetronomeComand = StopMetronome


timer :: Int -> IO () -> IO ()
timer x func = void $ forkIO $ do
    threadDelay x
    func


metronome :: Int -> IO () -> IO ()
metronome x func = void $ forkIO $ forever $ do
    threadDelay x
    func


metronomeS :: Int -> IO () -> IO ()
metronomeS x func = void $ forkIO $ forever $ do
    func
    threadDelay x


stopebleMetronome :: Int -> IO () -> IO (Chan MetronomeComand)
stopebleMetronome x func = do
    chan <- newChan
    void . forkIO . void $ race (metronome x func) (void $ readChan chan)
    return chan

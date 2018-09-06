{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Enecuum.Dsl.Graph where

import           Control.Concurrent.STM
import           Test.HUnit
import           Data.Maybe
import qualified Data.Set as S    
import qualified Data.Map as M

import qualified Enecuum.TGraph as G
import           Enecuum.StringHashable
import           Enecuum.Dsl.Graph.Interpreter
import           Enecuum.Dsl.Graph.Language


testNewNode :: Test
testNewNode = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        x <- getNode (toHash @Int 123)
        return $ case x of
            Just (Node _ _ c _) -> 123 == fromContent c
            Nothing     -> False
    assertBool "" aOk


testDeleteNode :: Test
testDeleteNode = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode (123 :: Int)
        deleteNode (123 :: Int)
        isNothing <$> getNode (toHash @Int 123)
    assertBool "" aOk



testNewLink :: Test
testNewLink = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode (123 :: Int)
        newNode (125 :: Int)
        newLink (toHash (123 :: Int)) (125 :: Int)
        x <- getNode (toHash @Int 123)
        case x of
            Just (Node _ _ _ l) -> return $ M.member (toHash @Int 125) l
            Nothing     -> return False
    assertBool "" aOk

--
testDeleteLink :: Test 
testDeleteLink = undefined

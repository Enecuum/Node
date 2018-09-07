{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Enecuum.Dsl.Graph where

import           Control.Concurrent.STM
import           Test.HUnit
import           Data.Maybe    
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
        x <- getNode (123 :: Int)
        return $ case x of
            Just (Node _ _ c _) -> 123 == fromContent c
            Nothing     -> False
    assertBool "" aOk


testGetNodeByHash :: Test
testGetNodeByHash = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        x <- getNode (toHash (123 :: Int))
        return $ case x of
            Just (Node _ _ c _) -> 123 == fromContent c
            Nothing     -> False
    assertBool "" aOk


testGetNodeByRef :: Test
testGetNodeByRef = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        x <- getNode (toHash (123 :: Int))
        case x of
            Just (Node _ ref _ _) -> do
                Just (Node _ _ c _) <- getNode ref
                return $ 123 == fromContent c
            Nothing     -> return False
    assertBool "" aOk



testDeleteNodeByContent :: Test
testDeleteNodeByContent = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode (123 :: Int)
        deleteNode (123 :: Int)
        isNothing <$> getNode (toHash @Int 123)
    assertBool "" aOk


testDeleteNodeByHash :: Test
testDeleteNodeByHash = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode (123 :: Int)
        deleteNode $ toHash (123 :: Int)
        isNothing <$> getNode (toHash @Int 123)
    assertBool "" aOk


testDeleteNodeByRef :: Test
testDeleteNodeByRef = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode (123 :: Int)
        aNode  <- getNode (toHash @Int 123)
        case aNode of
            (Just (Node _ aRef _ _)) -> do
                deleteNode aRef
                isNothing <$> getNode (123 :: Int)
            _ -> return False
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

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
        Just (Node _ _ c _ _) <- getNode (123 :: Int)
        return $ 123 == fromContent c
    assertBool "" aOk


testGetNodeByHash :: Test
testGetNodeByHash = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        Just (Node _ _ c _ _) <- getNode (toHash (123 :: Int))
        return $ 123 == fromContent c
    assertBool "" aOk


testGetNodeByRef :: Test
testGetNodeByRef = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        Just (Node _ ref _ _ _) <- getNode (toHash (123 :: Int))
        Just (Node _ _ c _ _)   <- getNode ref
        return $ 123 == fromContent c
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
        Just (Node _ aRef _ _ _) <- getNode (toHash @Int 123)
        deleteNode aRef
        isNothing <$> getNode (123 :: Int)
    assertBool "" aOk


testNewLinkByContent :: Test
testNewLinkByContent = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int) (125 :: Int)
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.member (toHash @Int 125) l
    assertBool "" aOk


testNewLinkByHash :: Test
testNewLinkByHash = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (toHash @Int 123) (toHash @Int 125)
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.member (toHash @Int 125) l
    assertBool "" aOk


testNewLinkByRef :: Test
testNewLinkByRef = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        Just (Node _ r1 _ _ _) <- getNode (toHash @Int 123)
        Just (Node _ r2 _ _ _) <- getNode (toHash @Int 125)
        newLink r1 r2
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.member (toHash @Int 125) l
    assertBool "" aOk


--
testDeleteLinkByContent :: Test 
testDeleteLinkByContent = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int) (125 :: Int)
        deleteLink (123 :: Int) (125 :: Int)
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.notMember (toHash @Int 125) l
    assertBool "" aOk



testDeleteLinkByHash :: Test 
testDeleteLinkByHash = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int) (125 :: Int)
        deleteLink (toHash @Int 123) (toHash @Int 125)
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.notMember (toHash @Int 125) l
    assertBool "" aOk


testDeleteLinkByRef :: Test 
testDeleteLinkByRef = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int) (125 :: Int)
        Just (Node _ r1 _ _ _) <- getNode (toHash @Int 123)
        Just (Node _ r2 _ _ _) <- getNode (toHash @Int 125)
        deleteLink r1 r2
        Just (Node _ _ _ l _) <- getNode (toHash @Int 123)
        return $ M.notMember (toHash @Int 125) l
    assertBool "" aOk
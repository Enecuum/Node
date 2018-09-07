{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Enecuum.Research.Dsl.HashGraph where

import           Control.Concurrent.STM
import           Test.HUnit
import           Data.Maybe    
import qualified Data.Map as M

import           Enecuum.Prelude

import qualified Enecuum.Research.THashGraph as G
import           Enecuum.Research.StringHashable
import           Enecuum.Research.Dsl.HashGraph.Interpreter
import           Enecuum.Research.Dsl.HashGraph.Language

hashGraphTestSuit = TestList
    [ TestLabel "Addition of new node / geting nod by content" testNewNode
    , TestLabel "Geting node by hash"                          testGetNodeByHash
    , TestLabel "Geting node by ref"                           testGetNodeByRef
    , TestLabel "Deleting of node by content" testDeleteNodeByContent
    , TestLabel "Deleting of node by hash" testDeleteNodeByHash
    , TestLabel "Deleting of node by ref" testDeleteNodeByRef
    , TestLabel "Addition of new Link by content"              testNewLinkByContent
    , TestLabel "Addition of new Link by hash" testNewLinkByHash
    , TestLabel "Addition of new Link by ref" testNewLinkByRef
    , TestLabel "Deleting of Link by content" testDeleteLinkByContent
    , TestLabel "Deleting of Link by hash" testDeleteLinkByHash
    , TestLabel "Deleting of Link by ref" testDeleteLinkByRef
    ]


testNewNode :: Test
testNewNode = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        Just (DslHashNode _ _ c _ _) <- getNode (123 :: Int64)
        return $ 123 == fromContent c
    assertBool "" aOk


testGetNodeByHash :: Test
testGetNodeByHash = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        Just (DslHashNode _ _ c _ _) <- getNode (toHash (123 :: Int64))
        return $ 123 == fromContent c
    assertBool "" aOk


testGetNodeByRef :: Test
testGetNodeByRef = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        Just (DslHashNode _ ref _ _ _) <- getNode (toHash (123 :: Int64))
        Just (DslHashNode _ _ c _ _)   <- getNode ref
        return $ 123 == fromContent c
    assertBool "" aOk



testDeleteNodeByContent :: Test
testDeleteNodeByContent = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode (123 :: Int64)
        deleteNode (123 :: Int64)
        isNothing <$> getNode (toHash @Int64 123)
    assertBool "" aOk


testDeleteNodeByHash :: Test
testDeleteNodeByHash = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode (123 :: Int64)
        deleteNode $ toHash (123 :: Int64)
        isNothing <$> getNode (toHash @Int64 123)
    assertBool "" aOk


testDeleteNodeByRef :: Test
testDeleteNodeByRef = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode (123 :: Int64)
        Just (DslHashNode _ aRef _ _ _) <- getNode (toHash @Int64 123)
        deleteNode aRef
        isNothing <$> getNode (123 :: Int64)
    assertBool "" aOk


testNewLinkByContent :: Test
testNewLinkByContent = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" aOk


testNewLinkByHash :: Test
testNewLinkByHash = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (toHash @Int64 123) (toHash @Int64 125)
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" aOk


testNewLinkByRef :: Test
testNewLinkByRef = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        Just (DslHashNode _ r1 _ _ _) <- getNode (toHash @Int64 123)
        Just (DslHashNode _ r2 _ _ _) <- getNode (toHash @Int64 125)
        newLink r1 r2
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" aOk


--
testDeleteLinkByContent :: Test 
testDeleteLinkByContent = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        deleteLink (123 :: Int64) (125 :: Int64)
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" aOk



testDeleteLinkByHash :: Test 
testDeleteLinkByHash = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        deleteLink (toHash @Int64 123) (toHash @Int64 125)
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" aOk


testDeleteLinkByRef :: Test 
testDeleteLinkByRef = TestCase $ do
    aNewGraph :: TVar (G.THashGraph Int64) <- initHashGraph
    aOk <- runHashGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        Just (DslHashNode _ r1 _ _ _) <- getNode (toHash @Int64 123)
        Just (DslHashNode _ r2 _ _ _) <- getNode (toHash @Int64 125)
        deleteLink r1 r2
        Just (DslHashNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" aOk
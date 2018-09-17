
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Enecuum.Core.HGraphSpec where

import           Control.Concurrent.STM
import           Test.HUnit
import           Data.Maybe
import qualified Data.Map as M

import           Enecuum.Prelude

import qualified Data.HGraph.THGraph as G
import           Data.HGraph.StringHashable
import           Enecuum.Core.HGraph.Interpreter          ( runHGraph, initHGraph )
import           Enecuum.Core.HGraph.Language
import           Enecuum.Core.HGraph.Types
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit                 ( fromHUnitTest )

spec :: Spec
spec = describe "HGraph eDSL tests" $ fromHUnitTest $ TestList
    [ TestLabel "Addition of new node / getting node by content" testNewNode
    , TestLabel "Getting node by hash" testGetNodeByHash
    , TestLabel "Getting node by ref" testGetNodeByRef
    , TestLabel "Deleting of node by content" testDeleteNodeByContent
    , TestLabel "Deleting of node by hash" testDeleteNodeByHash
    , TestLabel "Deleting of node by ref" testDeleteNodeByRef
    , TestLabel "Addition of new Link by content" testNewLinkByContent
    , TestLabel "Addition of new Link by hash" testNewLinkByHash
    , TestLabel "Addition of new Link by ref" testNewLinkByRef
    , TestLabel "Deleting of Link by content" testDeleteLinkByContent
    , TestLabel "Deleting of Link by hash" testDeleteLinkByHash
    , TestLabel "Deleting of Link by ref" testDeleteLinkByRef
    , TestLabel "List-like graph construction test" testListLikeGraph
    ]

testNewNode :: Test
testNewNode = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        Just (HNode _ _ c _ _) <- getNode (123 :: Int64)
        return $ 123 == fromContent c
    assertBool "" ok

testGetNodeByHash :: Test
testGetNodeByHash = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        Just (HNode _ _ c _ _) <- getNode (toHash (123 :: Int64))
        return $ 123 == fromContent c
    assertBool "" ok

testGetNodeByRef :: Test
testGetNodeByRef = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        Just (HNode _ ref _ _ _) <- getNode (toHash (123 :: Int64))
        Just (HNode _ _   c _ _) <- getNode ref
        return $ 123 == fromContent c
    assertBool "" ok

testDeleteNodeByContent :: Test
testDeleteNodeByContent = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode (123 :: Int64)
        deleteNode (123 :: Int64)
        isNothing <$> getNode (toHash @Int64 123)
    assertBool "" ok

testDeleteNodeByHash :: Test
testDeleteNodeByHash = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode (123 :: Int64)
        deleteNode $ toHash (123 :: Int64)
        isNothing <$> getNode (toHash @Int64 123)
    assertBool "" ok

testDeleteNodeByRef :: Test
testDeleteNodeByRef = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode (123 :: Int64)
        Just (HNode _ ref _ _ _) <- getNode (toHash @Int64 123)
        deleteNode ref
        isNothing <$> getNode (123 :: Int64)
    assertBool "" ok

testNewLinkByContent :: Test
testNewLinkByContent = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" ok

testNewLinkByHash :: Test
testNewLinkByHash = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newLink (toHash @Int64 123) (toHash @Int64 125)
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" ok

testNewLinkByRef :: Test
testNewLinkByRef = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        Just (HNode _ r1 _ _ _) <- getNode (toHash @Int64 123)
        Just (HNode _ r2 _ _ _) <- getNode (toHash @Int64 125)
        newLink r1 r2
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.member (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByContent :: Test
testDeleteLinkByContent = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        deleteLink (123 :: Int64) (125 :: Int64)
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByHash :: Test
testDeleteLinkByHash = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        deleteLink (toHash @Int64 123) (toHash @Int64 125)
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByRef :: Test
testDeleteLinkByRef = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        Just (HNode _ r1 _ _ _) <- getNode (toHash @Int64 123)
        Just (HNode _ r2 _ _ _) <- getNode (toHash @Int64 125)
        deleteLink r1 r2
        Just (HNode _ _ _ l _) <- getNode (toHash @Int64 123)
        return $ M.notMember (toHash @Int64 125) l
    assertBool "" ok

testListLikeGraph :: Test
testListLikeGraph = TestCase $ do
    newGraph :: TVar (G.THGraph Int64) <- initHGraph
    ok <- runHGraph newGraph $ do
        newNode 123
        newNode 125
        newNode 127
        newLink (toHash @Int64 123) (toHash @Int64 125)
        newLink (toHash @Int64 125) (toHash @Int64 127)
        Just (HNode _ _ _ l1 _) <- getNode (toHash @Int64 123)
        Just (HNode _ _ _ l2 _) <- getNode (toHash @Int64 125)
        return $ all id
          [ M.member (toHash @Int64 125) l1
          , M.member (toHash @Int64 127) l2
          , not (M.member (toHash @Int64 125) l2)
          , not (M.member (toHash @Int64 127) l1)
          ]
    assertBool "" ok

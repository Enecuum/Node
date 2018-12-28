
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Enecuum.Tests.Functional.HGraphSpec where

import qualified Data.Aeson                          as A
import qualified Data.Map                            as M
import           Data.Maybe
import           Test.HUnit

import           Enecuum.Domain                      as D
import           Enecuum.Prelude

import           Enecuum.Core.HGraph.Internal.Impl   (initHGraph)
import           Enecuum.Core.HGraph.Interpreters.IO (runHGraphIO)
import           Enecuum.Core.HGraph.Language

import           Enecuum.Testing.Wrappers
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit            (fromHUnitTest)
import           Test.QuickCheck                     (property)

data SomeStructure = SomeStructure
    { someData :: String
    , someInt  :: Int
    }
  deriving (Generic, Serialize)

instance StringHashable SomeStructure where
    toHash = toHashGeneric

spec :: Spec
spec = stableTest $ fastTest $ do
    describe "HGraph eDSL tests" $ fromHUnitTest $ TestList
        [ TestLabel "Addition of new node / getting node by content" testNewNode
        , TestLabel "Getting node by hash"                           testGetNodeByHash
        , TestLabel "Getting node by ref"                            testGetNodeByRef
        , TestLabel "Deleting of node by content"                    testDeleteNodeByContent
        , TestLabel "Deleting of node by hash"                       testDeleteNodeByHash
        , TestLabel "Deleting of node by ref"                        testDeleteNodeByRef
        , TestLabel "Addition of new Link by content"                testNewLinkByContent
        , TestLabel "Addition of new Link by hash"                   testNewLinkByHash
        , TestLabel "Addition of new Link by ref"                    testNewLinkByRef
        , TestLabel "Deleting of Link by content"                    testDeleteLinkByContent
        , TestLabel "Deleting of Link by hash"                       testDeleteLinkByHash
        , TestLabel "Deleting of Link by ref"                        testDeleteLinkByRef
        , TestLabel "List-like graph construction test"              testListLikeGraph
        ]
    describe "StringHash tests" $ do
        it "StringHash serialize / deserialize property test" $ property $ \rndStrHash ->
            let origHash     = StringHash $ encodeUtf8 (rndStrHash :: String)
                serialized   = A.encode origHash
                deserialized = A.decode serialized
            in  Just origHash == deserialized
        it "StringHash serialize / deserialize test" $ do
            let origHash     = toHash $ SomeStructure "abC5435" 232
            let serialized   = A.encode origHash
            let deserialized = A.decode serialized
            Just origHash `shouldBe` deserialized


testNewNode :: Test
testNewNode = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        (HNode _ _ c _ _) <- fromJust <$> getNode (123 :: Int64)
        pure $ 123 == fromContent c
    assertBool "" ok

testGetNodeByHash :: Test
testGetNodeByHash = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        (HNode _ _ c _ _) <- fromJust <$> getNode (toHash (123 :: Int64))
        pure $ 123 == fromContent c
    assertBool "" ok

testGetNodeByRef :: Test
testGetNodeByRef = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        (HNode _ ref _ _ _) <- fromJust <$> getNode (toHash (123 :: Int64))
        (HNode _ _   c _ _) <- fromJust <$> getNode ref
        pure $ 123 == fromContent c
    assertBool "" ok

testDeleteNodeByContent :: Test
testDeleteNodeByContent = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode (123 :: Int64)
        deleted <- deleteNode' (123 :: Int64)
        (&& deleted) . isNothing <$> getNode (toHash @Int64 123)
    assertBool "" ok

testDeleteNodeByHash :: Test
testDeleteNodeByHash = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode (123 :: Int64)
        deleted <- deleteNode' $ toHash (123 :: Int64)
        (&& deleted) . isNothing <$> getNode (toHash @Int64 123)
    assertBool "" ok

testDeleteNodeByRef :: Test
testDeleteNodeByRef = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode (123 :: Int64)
        (HNode _ ref _ _ _) <- fromJust <$> getNode (toHash @Int64 123)
        deleted <- deleteNode' ref
        (&& deleted) . isNothing <$> getNode (123 :: Int64)
    assertBool "" ok

testNewLinkByContent :: Test
testNewLinkByContent = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ M.member (toHash @Int64 125) l
    assertBool "" ok

testNewLinkByHash :: Test
testNewLinkByHash = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newLink (toHash @Int64 123) (toHash @Int64 125)
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ M.member (toHash @Int64 125) l
    assertBool "" ok

testNewLinkByRef :: Test
testNewLinkByRef = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        (HNode _ r1 _ _ _) <- fromJust <$> getNode (toHash @Int64 123)
        (HNode _ r2 _ _ _) <- fromJust <$> getNode (toHash @Int64 125)
        newLink r1 r2
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ M.member (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByContent :: Test
testDeleteLinkByContent = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newLink    (123 :: Int64) (125 :: Int64)
        deleted <- deleteLink' (123 :: Int64) (125 :: Int64)
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ deleted && M.notMember (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByHash :: Test
testDeleteLinkByHash = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newLink    (123 :: Int64)      (125 :: Int64)
        deleted <- deleteLink' (toHash @Int64 123) (toHash @Int64 125)
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ deleted && M.notMember (toHash @Int64 125) l
    assertBool "" ok

testDeleteLinkByRef :: Test
testDeleteLinkByRef = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newLink (123 :: Int64) (125 :: Int64)
        (HNode _ r1 _ _ _) <- fromJust <$> getNode (toHash @Int64 123)
        (HNode _ r2 _ _ _) <- fromJust <$> getNode (toHash @Int64 125)
        deleted <- deleteLink' r1 r2
        (HNode _ _ _ l _) <- fromJust <$> getNode (toHash @Int64 123)
        pure $ deleted && M.notMember (toHash @Int64 125) l
    assertBool "" ok

testListLikeGraph :: Test
testListLikeGraph = TestCase $ do
    newGraph :: D.TGraph Int64 <- initHGraph
    ok                         <- runHGraphIO newGraph $ do
        newNode 123
        newNode 125
        newNode 127
        newLink (toHash @Int64 123) (toHash @Int64 125)
        newLink (toHash @Int64 125) (toHash @Int64 127)
        (HNode _ _ _ l1 _) <- fromJust <$> getNode (toHash @Int64 123)
        (HNode _ _ _ l2 _) <- fromJust <$> getNode (toHash @Int64 125)
        pure $ and
            [ M.member (toHash @Int64 125) l1
            , M.member (toHash @Int64 127) l2
            , not (M.member (toHash @Int64 125) l2)
            , not (M.member (toHash @Int64 127) l1)
            ]
    assertBool "" ok

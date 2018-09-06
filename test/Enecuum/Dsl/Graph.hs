{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Enecuum.Dsl.Graph where

import           Control.Concurrent.STM
import           Test.HUnit
import           Data.Maybe
import qualified Data.Set as S    

import qualified Enecuum.TGraph as G
import           Enecuum.StringHashable
import           Enecuum.Dsl.Graph.Interpreter
import           Enecuum.Dsl.Graph.Language


testNewNode :: Test
testNewNode = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        x <- findNode (toHash @Int 123)
        return $ case x of
            Just (c, _) -> 123 == fromContent c
            Nothing     -> False
    assertBool "" aOk


testDeleteNode :: Test
testDeleteNode = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        deleteNode 123
        isNothing <$> findNode (toHash @Int 123)
    assertBool "" aOk


testDeleteRNode :: Test
testDeleteRNode = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        x <- findRNode (toHash @Int 123)
        case x of
            Just (r, _) -> do
                deleteRNode r
                isNothing <$> findNode (toHash @Int 123)
            Nothing     -> return False
        isNothing <$> findNode (toHash @Int 123)
    assertBool "" aOk


testNewLink :: Test
testNewLink = TestCase $ do
    aNewGraph :: TVar (G.TGraph Int) <- initGraph
    aOk <- runGraph aNewGraph $ do
        newNode 123
        newNode 125
        newLink 123 125
        x <- findNode (toHash @Int 123)
        case x of
            Just (_, l) -> return $ S.member (toHash @Int 125) l
            Nothing     -> return False
    assertBool "" aOk

--
testDeleteLink :: Test 
testDeleteLink = undefined
    {-

newLink, deleteLink :: c -> c -> Eff r ()
deleteRNode :: c -> Eff r ()
newRLink, deleteRLink :: c -> c -> Eff r ()
-}
{-# LANGUAGE NoImplicitPrelude #-}

module Enecuum.Dsl.Graph.Interpreter where

import           Universum
import           Enecuum.Dsl.Graph.Language

runGraph :: Eff '[GraphDsl] w -> IO w
runGraph = undefined
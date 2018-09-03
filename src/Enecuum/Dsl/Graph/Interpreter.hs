{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Enecuum.Dsl.Graph.Interpreter where

import           Universum
import Eff
import           Enecuum.Dsl.Graph.Language

runGraph :: Eff '[GraphDsl] w -> IO w
runGraph = undefined
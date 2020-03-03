{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Natural
  ( naturalPrims
  ) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text) 
import GHC.Natural

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.Evaluator.Common
import Clash.GHC.Evaluator.Convert

naturalPrims :: HashMap Text EvalPrim
naturalPrims = HashMap.fromList
  [ ("GHC.Natural.naturalToInteger", evalUnaryOp naturalToInteger)
  ]


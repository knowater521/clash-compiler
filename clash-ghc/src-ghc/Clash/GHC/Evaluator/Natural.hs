{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Natural
  ( naturalPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text) 
import GHC.Natural (naturalToInteger)

import Clash.Core.Evaluator.Models

import Clash.GHC.Evaluator.Strategy

naturalPrims :: HashMap Text EvalPrim
naturalPrims = HashMap.fromList
  [ ("GHC.Natural.naturalToInteger", evalUnaryOp naturalToInteger)
  ]


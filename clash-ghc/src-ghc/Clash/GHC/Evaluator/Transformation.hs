{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Transformation
  ( transformationPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.Evaluator.Common
import Clash.GHC.Evaluator.Strategy

transformationPrims :: HashMap Text EvalPrim
transformationPrims = HashMap.fromList
  [ ("Clash.Transformations.removedArg", evalId)
  ]


{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PrimEval.CString
  ( cStringPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.PrimEval.Common

cStringPrims :: HashMap Text EvalPrim
cStringPrims = HashMap.fromList
  [ ("GHC.CString.unpackCString#", evalId)
  ]


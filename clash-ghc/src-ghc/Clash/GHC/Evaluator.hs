module Clash.GHC.Evaluator
  ( evaluatePrimOp
  ) where

import Prelude hiding (pi)

import Control.Exception.Base
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe (unsafeDupablePerformIO)

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.Evaluator.Bit
import Clash.GHC.Evaluator.BitVector
import Clash.GHC.Evaluator.Char
import Clash.GHC.Evaluator.CString
import Clash.GHC.Evaluator.Double
import Clash.GHC.Evaluator.EnumTag
import Clash.GHC.Evaluator.Float
import Clash.GHC.Evaluator.Index
import Clash.GHC.Evaluator.Int
import Clash.GHC.Evaluator.Integer
import Clash.GHC.Evaluator.Narrowings
import Clash.GHC.Evaluator.Natural
import Clash.GHC.Evaluator.Signed
import Clash.GHC.Evaluator.Unsigned
import Clash.GHC.Evaluator.Word

-- If we try to evaluate a prim we haven't added an implementation for in the
-- primsMap, we simply return it back without trying to evaluate it. The only
-- time 'Nothing' is returned is when an attempt to evaluate a prim fails.
--
evaluatePrimOp :: EvalPrim
evaluatePrimOp pi args =
  case HashMap.lookup (primName pi) primsMap of
    -- TODO We should probably only 'evaluate' if the prim might throw an
    -- exception instead of on every prim.
    Just f ->
      unsafeDupablePerformIO $ evaluate (f pi args) `catch` errToUndefined 

    Nothing -> return (VPrim pi args)
 where
  errToUndefined DivideByZero =
    error "WHOOPS: DIVIDE BY ZERO"
  errToUndefined e = error
    $  "Unexpected "
    <> show e
    <> " when evalauting "
    <> show (primName pi)
    <> " with args "
    <> show args

  primsMap = mconcat
    [ bitPrims
    , bitVectorPrims
    , charPrims
    , cStringPrims
    , doublePrims
    , enumTagPrims
    , floatPrims
    , indexPrims
    , intPrims
    , integerPrims
    , narrowingPrims
    , naturalPrims
    , signedPrims
    , unsignedPrims
    , wordPrims
    ]


module Clash.GHC.Evaluator
  ( evaluatePrimOp
  ) where

import Prelude hiding (pi)

import Control.Exception.Base
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace (traceM)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Clash.Core.Evaluator.Delay
import Clash.Core.Evaluator.Models
import Clash.Core.Term
import Clash.Driver.Types (DebugLevel(DebugName))

import Clash.GHC.Evaluator.Bit
import Clash.GHC.Evaluator.BitVector
import Clash.GHC.Evaluator.Char
import Clash.GHC.Evaluator.CString
import Clash.GHC.Evaluator.Double
import Clash.GHC.Evaluator.EnumTag
import Clash.GHC.Evaluator.Float
import Clash.GHC.Evaluator.Int
import Clash.GHC.Evaluator.Integer
import Clash.GHC.Evaluator.Narrowings
import Clash.GHC.Evaluator.Word

-- If we try to evaluate a prim we haven't added an implementation for in the
-- primsMap, we simply return it back without trying to evaluate it. The only
-- time 'Nothing' is returned is when an attempt to evaluate a prim fails.
--
evaluatePrimOp :: EvalPrim
evaluatePrimOp env pi args =
  case HashMap.lookup (primName pi) primsMap of
    Just f ->
      unsafeDupablePerformIO $ evaluate (f env pi args) `catch` errToUndefined 

    -- TODO Warning on missing primitive.
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
    , intPrims
    , integerPrims
    , narrowingPrims
    , wordPrims
    ]

-- | Emits a warning to stderr using traceM. This is somewhat of a hack
-- to make up for the fact that clash lacks proper diagnostics.
--
warn :: DebugLevel -> PrimInfo -> Delay ()
warn level pi
  | level >= DebugName = do
      traceM ("No implementation for prim: " <> show (primName pi))
      return ()

  | otherwise = return ()


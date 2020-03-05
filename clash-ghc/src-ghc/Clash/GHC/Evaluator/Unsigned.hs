{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Unsigned
  ( unsignedPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.Evaluator.Strategy

unsignedPrims :: HashMap Text EvalPrim
unsignedPrims = HashMap.fromList
  [ -- General
    ("Clash.Sized.Internal.Unsigned.size#", evalMissing)

    -- BitPack
  , ("Clash.Sized.Internal.Unsigned.pack#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.unpack#", evalMissing)

    -- Eq
  , ("Clash.Sized.Internal.Unsigned.eq#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.neq#", evalMissing)

    -- Ord
  , ("Clash.Sized.Internal.Unsigned.lt#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.le#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.gt#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.ge#", evalMissing)

    -- Bound
  , ("Clash.Sized.Internal.Unsigned.minBound#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.maxBound#", evalMissing)

    -- Num
  , ("Clash.Sized.Internal.Unsigned.+#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.-#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.*#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.negate#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.fromInteger#", evalId)

    -- ExtendingNum
  , ("Clash.Sized.Internal.Unsigned.plus#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.minus#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.times#", evalMissing)

    -- Integral
  , ("Clash.Sized.Internal.Unsigned.quot#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.rem#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.toInteger#", evalMissing)

    -- Bits
  , ("Clash.Sized.Internal.Unsigned.and#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.or#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.xor#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.complement#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.shiftL#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.shiftR#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.rotateL#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.rotateR#", evalMissing)

    -- Resize
  , ("Clash.Sized.Internal.Unsigned.resize#", evalMissing)
  , ("Clash.Sized.Internal.Unsigned.truncateB#", evalMissing)
  ]


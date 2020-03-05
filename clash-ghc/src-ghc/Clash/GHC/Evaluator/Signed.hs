{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Signed
  ( signedPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.Evaluator.Strategy

signedPrims :: HashMap Text EvalPrim
signedPrims = HashMap.fromList
  [ -- General
    ("Clash.Sized.Internal.Signed.size#", evalMissing)

    -- BitPack
  , ("Clash.Sized.Internal.Signed.pack#", evalMissing)
  , ("Clash.Sized.Internal.Signed.unpack#", evalMissing)

    -- Eq
  , ("Clash.Sized.Internal.Signed.eq#", evalMissing)
  , ("Clash.Sized.Internal.Signed.neq#", evalMissing)

    -- Ord
  , ("Clash.Sized.Internal.Signed.lt#", evalMissing)
  , ("Clash.Sized.Internal.Signed.le#", evalMissing)
  , ("Clash.Sized.Internal.Signed.gt#", evalMissing)
  , ("Clash.Sized.Internal.Signed.ge#", evalMissing)

    -- Bound
  , ("Clash.Sized.Internal.Signed.minBound#", evalMissing)
  , ("Clash.Sized.Internal.Signed.maxBound#", evalMissing)

    -- Num
  , ("Clash.Sized.Internal.Signed.+#", evalMissing)
  , ("Clash.Sized.Internal.Signed.-#", evalMissing)
  , ("Clash.Sized.Internal.Signed.*#", evalMissing)
  , ("Clash.Sized.Internal.Signed.negate#", evalMissing)
  , ("Clash.Sized.Internal.Signed.abs#", evalMissing)
  , ("Clash.Sized.Internal.Signed.fromInteger#", evalId)

    -- ExtendingNum
  , ("Clash.Sized.Internal.Signed.plus#", evalMissing)
  , ("Clash.Sized.Internal.Signed.minus#", evalMissing)
  , ("Clash.Sized.Internal.Signed.times#", evalMissing)

    -- Integral
  , ("Clash.Sized.Internal.Signed.quot#", evalMissing)
  , ("Clash.Sized.Internal.Signed.rem#", evalMissing)
  , ("Clash.Sized.Internal.Signed.div#", evalMissing)
  , ("Clash.Sized.Internal.Signed.mod#", evalMissing)
  , ("Clash.Sized.Internal.Signed.toInteger#", evalMissing)

    -- Bits
  , ("Clash.Sized.Internal.Signed.and#", evalMissing)
  , ("Clash.Sized.Internal.Signed.or#", evalMissing)
  , ("Clash.Sized.Internal.Signed.xor#", evalMissing)
  , ("Clash.Sized.Internal.Signed.complement#", evalMissing)
  , ("Clash.Sized.Internal.Signed.shiftL#", evalMissing)
  , ("Clash.Sized.Internal.Signed.shiftR#", evalMissing)
  , ("Clash.Sized.Internal.Signed.rotateL#", evalMissing)
  , ("Clash.Sized.Internal.Signed.rotateR#", evalMissing)

    -- Resize
  , ("Clash.Sized.Internal.Signed.resize#", evalMissing)
  , ("Clash.Sized.Internal.Signed.truncateB#", evalMissing)
  ]


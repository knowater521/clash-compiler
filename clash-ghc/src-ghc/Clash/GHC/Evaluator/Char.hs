{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.Char
  ( charPrims
  ) where

import Prelude hiding (pi)

import qualified Control.Monad.State.Strict as State
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Prim
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Term

import Clash.GHC.Evaluator.Common
import Clash.GHC.Evaluator.Convert

-- | Primitive Operations defined on Char / Char#
--
charPrims :: HashMap Text EvalPrim
charPrims = HashMap.fromList
  [ ("GHC.Prim.gtChar#", evalComparison# gtChar#)
  , ("GHC.Prim.geChar#", evalComparison# geChar#)
  , ("GHC.Prim.eqChar#", evalComparison# eqChar#)
  , ("GHC.Prim.neChar#", evalComparison# neChar#)
  , ("GHC.Prim.ltChar#", evalComparison# ltChar#)
  , ("GHC.Prim.leChar#", evalComparison# leChar#)
  , ("GHC.Prim.ord#", primOrd)
  , ("GHC.Types.C#", primC)
  ]

primOrd :: EvalPrim
primOrd = evalUnaryOp $ \i ->
  let !(C# a) = i in I# (ord# a)

primC :: EvalPrim
primC pi args
  | Just [i] <- traverse fromValue (Either.lefts args)
  = do tcm <- State.gets envTcMap
       let ([], [charDc]) = typeInfo tcm (primType pi)
           !(C# a) = i
       
       return $ VData charDc
         [Left $ toValue tcm (primType pi) (C# a)]

  | otherwise
  = return (VPrim pi args)

evalComparison# :: (Char# -> Char# -> Int#) -> EvalPrim
evalComparison# op = evalBinaryOp $ \i j ->
  let !(C# a) = i
      !(C# b) = j
   in I# (a `op` b)


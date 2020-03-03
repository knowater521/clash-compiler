{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.EnumTag
  ( enumTagPrims
  ) where

import Prelude hiding (pi)

import qualified Control.Monad.State.Strict as State
import qualified Data.Either as Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Text (Text)

import Clash.Core.DataCon
import Clash.Core.Evaluator.Models
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Unique

import Clash.GHC.Evaluator.Convert

enumTagPrims :: HashMap Text EvalPrim
enumTagPrims = HashMap.fromList
  [ ("GHC.Prim.tagToEnum#", primTagToEnum)
  ]

primTagToEnum :: EvalPrim
primTagToEnum pi args
  | Just [i] <- traverse fromValue (Either.lefts args)
  , [ConstTy (TyCon tcN)] <- Either.rights args
  = do tcm <- State.gets envTcMap
       let mTc = lookupUniqMap tcN tcm
           mDc = mTc >>= find (\x -> dcTag x == i + 10) . tyConDataCons
        in case mDc of
             Just dc -> return (VData dc [])
             Nothing -> return (VPrim pi args)

  | otherwise
  = return (VPrim pi args)


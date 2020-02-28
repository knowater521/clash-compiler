{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.Evaluator.ByteArray
  ( byteArrayPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Core.Evaluator.Models

import Clash.GHC.Evaluator.Common
import Clash.GHC.Evaluator.Convert

byteArrayPrims :: HashMap Text EvalPrim
byteArrayPrims = HashMap.fromList
  [ -- Construction
    ("GHC.Prim.newByteArray#", evalMissing)

    -- Common Operations
  , ("GHC.Prim.sameMutableByteArray#", evalMissing)
  , ("GHC.Prim.shrinkMutableByteArray#", evalMissing)
  , ("GHC.Prim.resizeMutableByteArray#", evalMissing)
  , ("GHC.Prim.unsafeFreezeByteArray#", primUnsafeFreezeByteArray)
  , ("GHC.Prim.sizeofByteArray#", evalMissing)
  , ("GHC.Prim.sizeofMutableByteArray#", evalMissing)
  , ("GHC.Prim.getSizeofMutableByteArray#", evalMissing)

    -- Indexing
  , ("GHC.Prim.indexCharArray#", evalMissing)
  , ("GHC.Prim.indexWideCharArray#", evalMissing)
  , ("GHC.Prim.indexIntArray#", evalMissing)
  , ("GHC.Prim.indexWordArray#", evalMissing)
  , ("GHC.Prim.indexAddrArray#", evalMissing)
  , ("GHC.Prim.indexFloatArray#", evalMissing)
  , ("GHC.Prim.indexDoubleArray#", evalMissing)
  , ("GHC.Prim.indexStablePtrArray#", evalMissing)
  , ("GHC.Prim.indexInt8Array#", evalMissing)
  , ("GHC.Prim.indexInt16Array#", evalMissing)
  , ("GHC.Prim.indexInt32Array#", evalMissing)
  , ("GHC.Prim.indexInt64Array#", evalMissing)
  , ("GHC.Prim.indexWord8Array#", evalMissing)
  , ("GHC.Prim.indexWord16Array#", evalMissing)
  , ("GHC.Prim.indexWord32Array#", evalMissing)
  , ("GHC.Prim.indexWord64Array#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsChar#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsWideChar#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsAddr#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsFloat#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsDouble#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsStablePtr#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsInt16#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsInt32#", evalMissing)
  , ("GHC.Prim.indexword8ArrayAsInt64#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsInt#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsWord16#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsWord32#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsWord64#", evalMissing)
  , ("GHC.Prim.indexWord8ArrayAsWord#", evalMissing)

    -- Reading
  , ("GHC.Prim.readCharArray#", evalMissing)
  , ("GHC.Prim.readWideCharArray#", evalMissing)
  , ("GHC.Prim.readIntArray#", evalMissing)
  , ("GHC.Prim.readWordArray#", evalMissing)
  , ("GHC.Prim.readAddrArray#", evalMissing)
  , ("GHC.Prim.readFloatArray#", evalMissing)
  , ("GHC.Prim.readDoubleArray#", evalMissing)
  , ("GHC.Prim.readStablePtrArray#", evalMissing)
  , ("GHC.Prim.readInt8Array#", evalMissing)
  , ("GHC.Prim.readInt16Array#", evalMissing)
  , ("GHC.Prim.readInt32Array#", evalMissing)
  , ("GHC.Prim.readInt64Array#", evalMissing)
  , ("GHC.Prim.readWord8Array#", evalMissing)
  , ("GHC.Prim.readWord16Array#", evalMissing)
  , ("GHC.Prim.readWord32Array#", evalMissing)
  , ("GHC.Prim.readWord64Array#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsChar#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsWideChar#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsAddr#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsFloat#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsDouble#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsStablePtr#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsInt16#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsInt32#", evalMissing)
  , ("GHC.Prim.readword8ArrayAsInt64#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsInt#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsWord16#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsWord32#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsWord64#", evalMissing)
  , ("GHC.Prim.readWord8ArrayAsWord#", evalMissing)

    -- Writing
  , ("GHC.Prim.writeCharArray#", evalMissing)
  , ("GHC.Prim.writeWideCharArray#", evalMissing)
  , ("GHC.Prim.writeIntArray#", evalMissing)
  , ("GHC.Prim.writeWordArray#", evalMissing)
  , ("GHC.Prim.writeAddrArray#", evalMissing)
  , ("GHC.Prim.writeFloatArray#", evalMissing)
  , ("GHC.Prim.writeDoubleArray#", evalMissing)
  , ("GHC.Prim.writeStablePtrArray#", evalMissing)
  , ("GHC.Prim.writeInt8Array#", evalMissing)
  , ("GHC.Prim.writeInt16Array#", evalMissing)
  , ("GHC.Prim.writeInt32Array#", evalMissing)
  , ("GHC.Prim.writeInt64Array#", evalMissing)
  , ("GHC.Prim.writeWord8Array#", evalMissing)
  , ("GHC.Prim.writeWord16Array#", evalMissing)
  , ("GHC.Prim.writeWord32Array#", evalMissing)
  , ("GHC.Prim.writeWord64Array#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsChar#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsWideChar#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsAddr#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsFloat#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsDouble#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsStablePtr#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsInt16#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsInt32#", evalMissing)
  , ("GHC.Prim.writeword8ArrayAsInt64#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsInt#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsWord16#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsWord32#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsWord64#", evalMissing)
  , ("GHC.Prim.writeWord8ArrayAsWord#", evalMissing)

    -- Copying
  , ("GHC.Prim.copyByteArray#", evalMissing)
  , ("GHC.Prim.copyMutableByteArray#", evalMissing)
  , ("GHC.Prim.copyByteArrayToAddr#", evalMissing)
  , ("GHC.Prim.copyMutableByteArrayToAddr#", evalMissing)
  , ("GHC.Prim.copyAddrToByteArray#", evalMissing)

    -- Misc. Operations.
  , ("GHC.Prim.setByteArray#", evalMissing)
  , ("GHC.Prim.atomicReadIntArray#", evalMissing)
  , ("GHC.Prim.atomicWriteIntArray#", evalMissing)
  , ("GHC.Prim.casIntArray#", evalMissing)

    -- Fetching
  , ("GHC.Prim.fetchAddIntArray#", evalMissing)
  , ("GHC.Prim.fetchSubIntArray#", evalMissing)
  , ("GHC.Prim.fetchAndIntArray#", evalMissing)
  , ("GHC.Prim.fetchNandIntArray#", evalMissing)
  , ("GHC.Prim.fetchOrIntArray#", evalMissing)
  , ("GHC.Prim.fetchXorIntArray#", evalMissing)
  ]

primUnsafeFreezeByteArray :: EvalPrim
primUnsafeFreezeByteArray = undefined


{-# LANGUAGE FlexibleContexts #-}

module Data.Attoparsec.IEEE754 (

  -- * Big-endian
    anyFloat32be
  , anyFloat64be

  ) where

import Control.Applicative ((<$>))
import Control.Monad.ST (ST, runST)
import Data.Array.ST (MArray, STUArray, newArray, castSTUArray, readArray)
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.Word (Word32, Word64)

------------------------------------------------------------------------
-- Parsing Floats/Doubles

-- | Read a 32-bit float in big endian format
anyFloat32be :: Parser Float
anyFloat32be = wordToFloat <$> anyWord32be

-- | Read a 64-bit float in big endian format
anyFloat64be :: Parser Double
anyFloat64be = wordToDouble <$> anyWord64be

------------------------------------------------------------------------
-- Conversions

-- | Interpret a 32-bit word as a 32-bit float.
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

-- | Interpret a 64-bit word as a 64-bit float.
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

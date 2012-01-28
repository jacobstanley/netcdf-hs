module Data.NetCDF.Types where

import Data.ByteString (ByteString)
import Data.Word (Word32, Word64)
import Data.Int (Int16, Int32)

------------------------------------------------------------------------
-- NetCDF Headers

-- | A NetCDF file header.
data Header = Header Format NumRecs [Dim] [Attr] [Var]
  deriving (Show)

-- | The format of a NetCDF file.
data Format = FormatClassic -- ^ file offsets are 32-bit
            | Format64Bit   -- ^ file offsets are 64-bit
  deriving (Show)

type Offset = Word64

-- | The current number of records stored in the unlimited dimension.
data NumRecs = Streaming -- ^ record count is unknown
             | NumRecs Word32
  deriving (Show)

-- | A dimension, attribute or variable name.
type Name = ByteString


------------------------------------------------------------------------
-- Dimensions

-- | A dimension (such as time, x, y, etc).
data Dim = Dim Name DimLength
  deriving (Show)

-- | The length of a dimension. There can be at most one unlimited
-- dimension.
data DimLength = Fixed Word32 | Unlimited
  deriving (Show)


------------------------------------------------------------------------
-- Attributes

-- | An attribute (such as history, long_name, units, _FillValue, etc).
data Attr = AttrByte   Name ByteString
          | AttrChar   Name ByteString
          | AttrShort  Name [Int16]
          | AttrInt    Name [Int32]
          | AttrFloat  Name [Float]
          | AttrDouble Name [Double]
  deriving (Show)


------------------------------------------------------------------------
-- Variables

-- | A variable (such as time, height, density, etc). The number of
-- dimensions indicates the dimensionality (rank) of the variable:
-- 0 for scalar, 1 for vector, 2 for matrix, etc.
data Var = Var Name [Dim] [Attr] NCType VarSize VarBegin
  deriving (Show)

-- | The type of a NetCDF value.
data NCType = NCByte | NCChar | NCShort | NCInt | NCFloat | NCDouble
  deriving (Show)

-- | The size of a variable in bytes.
type VarSize  = Word32

-- | The byte offset at which a variable begins in the file.
type VarBegin = Offset

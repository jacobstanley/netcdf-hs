module Data.NetCDF.Types where

import Data.ByteString (ByteString)
import Data.Word (Word32, Word64)
import Data.Int (Int16, Int32)

------------------------------------------------------------------------
-- NetCDF Header

-- | A NetCDF file header.
data Header = Header {

  -- | The format of the file, either classic or 64-bit.
    hdrFormat  :: Format

  -- | The number of records in the file.
  , hdrNumRecs :: NumRecs

  -- | A list of the dimensions used by the variables.
  , hdrDims    :: [Dim]

  -- | Global attributes which apply to the entire file.
  , hdrAttrs   :: [Attr]

  -- | A list of the variables stored in the file.
  , hdrVars    :: [Var]

  } deriving (Show)

-- | The format of a NetCDF file.
data Format =
    FormatClassic -- ^ File offsets are 32-bit.
  | Format64Bit   -- ^ File offsets are 64-bit.
  deriving (Show)

-- | The current number of records stored in the unlimited dimension.
data NumRecs =
    Streaming      -- ^ The record count is unknown.
  | NumRecs Word32 -- ^ The number of records in the file.
  deriving (Show)

-- | A dimension, attribute or variable name stored in UTF-8.
type Name = ByteString


------------------------------------------------------------------------
-- Dimensions

-- | A dimension (such as time, x, y, etc).
data Dim = Dim
  { dimName   :: Name
  , dimLength :: DimLength
  } deriving (Show)

-- | The length of a dimension. There can be at most one unlimited
-- dimension. The unlimited dimension is also known as the record
-- dimension.
data DimLength =
    Fixed Word32 -- ^ The dimension has a fixed number of values.
  | Unlimited    -- ^ The dimension has an unlimited number of values.
  deriving (Show)


------------------------------------------------------------------------
-- Attributes

-- | An attribute (such as history, long_name, units, _FillValue, etc).
data Attr = Attr {
    attrName  :: Name
  , attrValue :: AttrValue
  } deriving (Show)

-- | The value of an attribute.
data AttrValue =
    AttrByte   ByteString -- ^ Raw bytes.
  | AttrChar   ByteString -- ^ UTF-8 string.
  | AttrShort  [Int16]
  | AttrInt    [Int32]
  | AttrFloat  [Float]
  | AttrDouble [Double]
  deriving (Show)


------------------------------------------------------------------------
-- Variables

-- | A variable (such as time, height, density, etc). The number of
-- dimensions indicates the dimensionality (rank) of the variable:
-- 0 for scalar, 1 for vector, 2 for matrix, etc.
data Var = Var {
  -- | The name of the variable.
    varName  :: Name

  -- | The applicable dimensions. The order of the dimensions relates
  -- to how to values are stored in the data section of the file.
  -- Values are stored in row-major order so the last dimension in the
  -- list varies the fastest.
  , varDims  :: [Dim]

  -- | The attributes for the variable such as the units the values
  -- are stored in or the long name of the variable.
  , varAttrs :: [Attr]

  -- | The type of the variable.
  , varType  :: VarType

  -- | The total size of the variable's values (in bytes), or the size
  -- of one record (in bytes) if the variable is in the unlimited
  -- dimension.
  , varSize  :: Word32

  -- | The offset from the start of the file at which the first value of
  -- this variable begins.
  , varBegin :: FileOffset

  } deriving (Show)

-- | An offset in to the NetCDF file.
type FileOffset = Word64

-- | The type of a variable.
data VarType =
    VarByte   -- ^ 8-bit byte.
  | VarChar   -- ^ 8-bit character, multi-byte encodings are allowed.
  | VarShort  -- ^ 16-bit signed integer, big endian, two's complement.
  | VarInt    -- ^ 32-bit signed integer, big endian, two's complement.
  | VarFloat  -- ^ 32-bit IEEE single precision, big endian.
  | VarDouble -- ^ 64-bit IEEE double precision, big endian.
  deriving (Show)

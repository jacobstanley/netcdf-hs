{-# LANGUAGE OverloadedStrings #-}

module Data.NetCDF.Serialize (getHeader) where

import           Control.Applicative
import           Data.Bits ((.&.), complement)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Int (Int16, Int32)
import           Data.Serialize
import           Data.Word (Word8, Word32)
import           Prelude hiding (take, drop)

import           Data.NetCDF.Types

------------------------------------------------------------------------

-- | Parse the header of a NetCDF file.
getHeader :: Get Header
getHeader = do
    string "CDF"

    format <- pure FormatClassic <* word8 1
          <|> pure Format64Bit   <* word8 2
          <?> "file format"

    numrecs <- pure Streaming <* word32be 0xFFFFFFFF
           <|> NumRecs <$> getWord32be

    dims  <- getDimensionList
    gatts <- getAttributeList
    vars  <- getVariableList dims (getOffsetFor format)

    return (Header format numrecs dims gatts vars)

-- | Create a parser for file offsets depending on the file format.
getOffsetFor :: Format -> Get FileOffset
getOffsetFor FormatClassic = fromIntegral <$> getWord32be
getOffsetFor Format64Bit   = fromIntegral <$> getWord64be

getDimensionList :: Get [Dim]
getDimensionList = headerList 0xA getDimension <?> "dimensions"

getAttributeList :: Get [Attr]
getAttributeList = headerList 0xC getAttribute <?> "attributes"

getVariableList  :: [Dim] -> Get FileOffset -> Get [Var]
getVariableList dims getOffset = headerList 0xB (getVariable dims getOffset) <?> "variables"

headerList :: Word32 -> Get a -> Get [a]
headerList tag gelem = absent <|> word32be tag *> getList gelem
  where
    absent = zero *> zero *> pure []
    zero   = word32be 0

getName :: Get Name
getName = getAlignedBytes <?> "name"


getDimension :: Get Dim
getDimension = Dim <$> getName <*> getDimensionLength

getDimensionLength :: Get DimLength
getDimensionLength = pure Unlimited <* word32be 0
                 <|> Fixed <$> getWord32be


getAttribute :: Get Attr
getAttribute = Attr <$> getName <*> getAttributeValue <?> "attribute"

getAttributeValue :: Get AttrValue
getAttributeValue = do
    typ <- getVariableType
    case typ of
      VarByte   -> AttrByte   <$> getAlignedBytes
      VarChar   -> AttrChar   <$> getAlignedBytes
      VarShort  -> AttrShort  <$> getAlignedShorts
      VarInt    -> AttrInt    <$> getList getInt32be
      VarFloat  -> AttrFloat  <$> getList getFloat32be
      VarDouble -> AttrDouble <$> getList getFloat64be


getVariable :: [Dim] -> Get FileOffset -> Get Var
getVariable dims getOffset =
    Var <$> getName
        <*> getList ((dims !!) <$> nonNeg)
        <*> getAttributeList
        <*> getVariableType
        <*> getWord32be
        <*> getOffset
        <?> "variable"

getVariableType :: Get VarType
getVariableType = do
    typ <- getWord32be
    case typ of
      1 -> pure VarByte
      2 -> pure VarChar
      3 -> pure VarShort
      4 -> pure VarInt
      5 -> pure VarFloat
      6 -> pure VarDouble
      _ -> fail ("unsupported nc_type = " ++ show typ)
    <?> "variableType"


------------------------------------------------------------------------
-- Utils

-- | Label the parser, in case failure occurs.
(<?>) :: Get a -> String -> Get a
g <?> lbl = label lbl g
infix 0 <?>

-- | Apply the given action repeatedly, returning every result.
count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)

-- | Expect a specific string.
string :: ByteString -> Get ()
string xs = expectS B.unpack (getByteString $ B.length xs) xs

-- | Expect a specific 8-bit word.
word8 :: Word8 -> Get ()
word8 = expect getWord8

-- | Expect a specific 32-bit big endian word.
word32be :: Word32 -> Get ()
word32be = expect getWord32be

expect :: (Show a, Eq a) => Get a -> a -> Get ()
expect = expectS show

expectS :: Eq a => (a -> String) -> Get a -> a -> Get ()
expectS show' get' x = get' >>= \y ->
    if x == y
       then return ()
       else fail ("expected " ++ show' x ++ " (was " ++ show' y ++ ")")

getAlignedBytes :: Get ByteString
getAlignedBytes = do
    n <- nonNeg
    getBytes n <* align n
    <?> "aligned bytes"

getAlignedShorts :: Get [Int16]
getAlignedShorts = do
    n <- nonNeg
    count n getInt16be <* align (n * 2)
    <?> "aligned shorts"

-- | Re-aligns the current offset to a 4-byte boundary given
-- the number of bytes read since the last alignment.
align :: Int -> Get ()
align bytesRead = getBytes padding *> pure () <?> "align"
  where
    boundary = (bytesRead + 3) .&. complement 0x3
    padding  = boundary - bytesRead

getList :: Get a -> Get [a]
getList g = do
    n <- nonNeg
    count n g

nonNeg :: Get Int
nonNeg = fromIntegral <$> getWord32be

getInt16be :: Get Int16
getInt16be = fromIntegral <$> getWord16be

getInt32be :: Get Int32
getInt32be = fromIntegral <$> getWord32be

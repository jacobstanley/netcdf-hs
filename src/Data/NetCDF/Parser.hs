{-# LANGUAGE OverloadedStrings #-}

module Data.NetCDF.Parser (header) where

import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be, word32be)
import Data.Attoparsec.IEEE754 (anyFloat32be, anyFloat64be)
import Data.Bits ((.&.), complement)
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32)
import Data.Word (Word32)
import Prelude hiding (take, drop)

import Data.NetCDF.Types

------------------------------------------------------------------------

-- | Parse the header of a NetCDF file.
header :: Parser Header
header = do
    string "CDF"

    format <- pure FormatClassic <* word8 1
          <|> pure Format64Bit   <* word8 2

    numrecs <- pure Streaming <* word32be 0xFFFFFFFF
           <|> NumRecs <$> anyWord32be

    dims  <- dimensionList
    gatts <- attributeList
    vars  <- variableList dims (offsetParser format)

    return (Header format numrecs dims gatts vars)

-- | Create a parser for file offsets depending on the file format.
offsetParser :: Format -> Parser Offset
offsetParser FormatClassic  = fromIntegral <$> anyWord32be
offsetParser Format64Bit = anyWord64be

dimensionList :: Parser [Dim]
dimensionList = headerList 0xA dimension <?> "dimensions"

attributeList :: Parser [Attr]
attributeList = headerList 0xC attribute <?> "attributes"

variableList  :: [Dim] -> Parser Offset -> Parser [Var]
variableList dims offsetP = headerList 0xB (variable dims offsetP) <?> "variables"

headerList :: Word32 -> Parser a -> Parser [a]
headerList tag pelem = absent <|> word32be tag *> list pelem
  where
    absent = zero *> zero *> pure []
    zero   = word32be 0

dimension :: Parser Dim
dimension = Dim <$> name <*> dimensionLength

dimensionLength :: Parser DimLength
dimensionLength = pure Unlimited <* word32be 0
              <|> Fixed <$> anyWord32be

variable :: [Dim] -> Parser Offset -> Parser Var
variable dims offsetP = Var
                    <$> name
                    <*> list ((dims !!) <$> nonNeg)
                    <*> attributeList
                    <*> nctype
                    <*> anyWord32be
                    <*> offsetP
                    <?> "variable"

attribute :: Parser Attr
attribute = do
    nam <- name
    typ <- nctype
    case typ of
      NCByte   -> AttrByte   nam <$> alignedBytes
      NCChar   -> AttrChar   nam <$> alignedBytes
      NCShort  -> AttrShort  nam <$> alignedShorts
      NCInt    -> AttrInt    nam <$> list anyInt32be
      NCFloat  -> AttrFloat  nam <$> list anyFloat32be
      NCDouble -> AttrDouble nam <$> list anyFloat64be
    <?> "attribute"

name :: Parser Name
name = alignedBytes <?> "name"

nctype :: Parser NCType
nctype = do
    typ <- anyWord32be
    case typ of
      1 -> pure NCByte
      2 -> pure NCChar
      3 -> pure NCShort
      4 -> pure NCInt
      5 -> pure NCFloat
      6 -> pure NCDouble
      _ -> fail ("unsupported nc_type = " ++ show typ)
    <?> "type"

------------------------------------------------------------------------
-- Utils

alignedBytes :: Parser ByteString
alignedBytes = do
    n <- nonNeg
    take n <* align n
    <?> "alignedBytes"

alignedShorts :: Parser [Int16]
alignedShorts = do
    n <- nonNeg
    count n anyInt16be <* align (n * 2)
    <?> "alignedShorts"

-- | Re-aligns the current offset to a 4-byte boundary given
-- the number of bytes read since the last alignment.
align :: Int -> Parser ()
align bytesRead = take padding *> pure () <?> "align"
  where
    boundary = (bytesRead + 3) .&. complement 0x03
    padding  = boundary - bytesRead

list :: Parser a -> Parser [a]
list p = do
    n <- nonNeg
    count n p

nonNeg :: Parser Int
nonNeg = fromIntegral <$> anyWord32be

anyInt16be :: Parser Int16
anyInt16be = fromIntegral <$> anyWord16be

anyInt32be :: Parser Int32
anyInt32be = fromIntegral <$> anyWord32be

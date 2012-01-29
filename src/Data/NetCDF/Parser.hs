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
offsetParser :: Format -> Parser FileOffset
offsetParser FormatClassic  = fromIntegral <$> anyWord32be
offsetParser Format64Bit = anyWord64be

dimensionList :: Parser [Dim]
dimensionList = headerList 0xA dimension <?> "dimensions"

attributeList :: Parser [Attr]
attributeList = headerList 0xC attribute <?> "attributes"

variableList  :: [Dim] -> Parser FileOffset -> Parser [Var]
variableList dims offsetP = headerList 0xB (variable dims offsetP) <?> "variables"

headerList :: Word32 -> Parser a -> Parser [a]
headerList tag pelem = absent <|> word32be tag *> list pelem
  where
    absent = zero *> zero *> pure []
    zero   = word32be 0

name :: Parser Name
name = alignedBytes <?> "name"


dimension :: Parser Dim
dimension = Dim <$> name <*> dimensionLength

dimensionLength :: Parser DimLength
dimensionLength = pure Unlimited <* word32be 0
              <|> Fixed <$> anyWord32be


attribute :: Parser Attr
attribute = Attr <$> name <*> attributeValue <?> "attribute"

attributeValue :: Parser AttrValue
attributeValue = do
    typ <- variableType
    case typ of
      VarByte   -> AttrByte   <$> alignedBytes
      VarChar   -> AttrChar   <$> alignedBytes
      VarShort  -> AttrShort  <$> alignedShorts
      VarInt    -> AttrInt    <$> list anyInt32be
      VarFloat  -> AttrFloat  <$> list anyFloat32be
      VarDouble -> AttrDouble <$> list anyFloat64be


variable :: [Dim] -> Parser FileOffset -> Parser Var
variable dims offsetP =
    Var <$> name
        <*> list ((dims !!) <$> nonNeg)
        <*> attributeList
        <*> variableType
        <*> anyWord32be
        <*> offsetP
        <?> "variable"

variableType :: Parser VarType
variableType = do
    typ <- anyWord32be
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

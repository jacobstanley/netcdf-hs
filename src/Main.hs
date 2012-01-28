{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be, word32be)
import Data.Attoparsec.IEEE754 (anyFloat32be, anyFloat64be)
import Data.Bits ((.&.), complement)
import Data.ByteString (ByteString)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary (sourceFile)
import Data.Int (Int16, Int32)
import Data.Word (Word32, Word64)

import Prelude hiding (take, drop)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = runResourceT $ do
    nc <- file $$ parser
    liftIO (putStrLn $ ppShow nc)
  where
    file   = sourceFile "/home/jake/data/netcdf/sresa1b_ncar_ccsm3_0_run1_200001.nc"
    parser = sinkParser netcdf

------------------------------------------------------------------------

data NetCDF = NetCDF Version NumRecs [Dim] [Attr] [Var]
  deriving (Show)

data Version = Classic | Offset64
  deriving (Show)

type Offset = Word64

data NumRecs = NumRecs Word32 | Streaming
  deriving (Show)

type Name = ByteString

data Dim = Dim Name DimSize
  deriving (Show)

data DimSize = Fixed Int | Unlimited
  deriving (Show)

data Var = Var Name [Dim] [Attr] NCType VarSize VarBegin
  deriving (Show)

type VarSize  = Int32
type VarBegin = Offset

data Attr = AttrByte   Name ByteString
          | AttrChar   Name ByteString
          | AttrShort  Name [Int16]
          | AttrInt    Name [Int32]
          | AttrFloat  Name [Float]
          | AttrDouble Name [Double]
  deriving (Show)

data NCType = NCByte | NCChar | NCShort | NCInt | NCFloat | NCDouble
  deriving (Show)

netcdf :: Parser NetCDF
netcdf = do
    string "CDF"

    version <- word8 1 *> pure Classic
           <|> word8 2 *> pure Offset64

    numrecs <- word32be 0xFFFFFFFF *> pure Streaming
           <|> NumRecs <$> anyWord32be

    dims  <- dimensionList
    gatts <- attributeList
    vars  <- variableList dims (offsetParser version)

    return (NetCDF version numrecs dims gatts vars)

offsetParser :: Version -> Parser Offset
offsetParser Classic  = fromIntegral <$> anyWord32be
offsetParser Offset64 = anyWord64be

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
dimension = Dim <$> name <*> dimensionSize

dimensionSize :: Parser DimSize
dimensionSize = do
    n <- nonNeg
    case n of
      0 -> pure Unlimited
      _ -> pure (Fixed n)

variable :: [Dim] -> Parser Offset -> Parser Var
variable dims offsetP = Var
       <$> name
       <*> list ((dims !!) <$> nonNeg)
       <*> attributeList
       <*> nctype
       <*> anyInt32be
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

list :: Parser a -> Parser [a]
list p = do
    n <- nonNeg
    count n p

name :: Parser Name
name = alignedBytes <?> "name"

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

nonNeg :: Parser Int
nonNeg = fromIntegral <$> anyWord32be

anyInt16be :: Parser Int16
anyInt16be = fromIntegral <$> anyWord16be

anyInt32be :: Parser Int32
anyInt32be = fromIntegral <$> anyWord32be

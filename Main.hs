{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), complement)
import Data.Attoparsec
import Data.Attoparsec.Binary (anyWord32be, anyWord64be)
import Data.ByteString (ByteString)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Binary (sourceFile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Word (Word32, Word64)

import Prelude hiding (take, drop)

main :: IO ()
main = runResourceT $ do
    n <- file $$ parser
    liftIO (print n)
  where
    file   = sourceFile "../perf/test01.nc"
    parser = sinkParser netcdf

------------------------------------------------------------------------

data NetCDF = NetCDF Version NumRecs [Dim] [Attr] [Var]
  deriving (Show)

data Version = Classic | Offset64
  deriving (Show)

data NumRecs = NumRecs Word32 | Streaming
  deriving (Show)

type Name = ByteString

data Dim = Dim Name Int
  deriving (Show)

data Attr = Attr
  deriving (Show)

newtype DimId = DimId Int
  deriving (Show)

type VSize  = Int
type VBegin = Word64

data Var = Var Name [DimId] [Attr] NCType VSize VBegin
  deriving (Show)

data NCType = NCByte | NCChar | NCShort | NCInt | NCFloat | NCDouble
  deriving (Show)

netcdf :: Parser NetCDF
netcdf = do
    string "CDF"

    version <- word8 1 *> pure Classic
           <|> word8 2 *> pure Offset64

    numrecs <- string "\xFF\xFF\xFF\xFF" *> pure Streaming
           <|> NumRecs <$> anyWord32be

    dims  <- dimensionList
    gatts <- attributeList
    vars  <- variableList

    return (NetCDF version numrecs dims gatts vars)

dimensionList :: Parser [Dim]
dimensionList = headerList "\x00\x00\x00\x0A" dimension

variableList  :: Parser [Var]
variableList  = headerList "\x00\x00\x00\x0B" variable

attributeList :: Parser [Attr]
attributeList = headerList "\x00\x00\x00\x0C" undefined

headerList :: ByteString -> Parser a -> Parser [a]
headerList tag pelem = absent <|> string tag *> list pelem
  where
    absent = string "\x00\x00\x00\x00\x00\x00\x00\x00" *> pure []

list :: Parser a -> Parser [a]
list p = do
    n <- nonNeg
    count n p

dimension :: Parser Dim
dimension = Dim <$> name <*> nonNeg

variable :: Parser Var
variable = Var
       <$> name
       <*> list (DimId <$> nonNeg) -- dimension ids
       <*> attributeList
       <*> nctype
       <*> nonNeg
       <*> anyWord64be

nctype :: Parser NCType
nctype = choice
    [ string "\x00\x00\x00\x01" *> pure NCByte
    , string "\x00\x00\x00\x02" *> pure NCChar
    , string "\x00\x00\x00\x03" *> pure NCShort
    , string "\x00\x00\x00\x04" *> pure NCInt
    , string "\x00\x00\x00\x05" *> pure NCFloat
    , string "\x00\x00\x00\x06" *> pure NCDouble ]

name :: Parser Name
name = do
    n <- nonNeg
    take n <* align n

-- | Re-aligns the current offset to a 4-byte boundary given
-- the number of bytes read since the last alignment.
align :: Int -> Parser ()
align bytesRead = take padding *> pure ()
  where
    boundary = (bytesRead + 3) .&. complement 0x03
    padding  = boundary - bytesRead

nonNeg :: Parser Int
nonNeg = fromIntegral <$> anyWord32be

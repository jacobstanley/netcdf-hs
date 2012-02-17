{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bits ((.&.), complement)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Conduit (($$), runResourceT)
import           Data.Conduit.Cereal (sinkGet)
import           Data.Conduit.Binary (sourceFile)
import           Data.Layout
import           Data.List hiding (repeat, group)
import qualified Data.Vector.Storable as V
import           Prelude hiding (repeat)
import           System.IO (IOMode(..), SeekMode(..), openFile, hSeek, hClose)

import           Data.NetCDF.Serialize (getHeader)
import           Data.NetCDF.Types

------------------------------------------------------------------------

main :: IO ()
main = do
    hdr <- parseHeader file
    nc  <- openFile file ReadMode

    let (NumRecs recs) = hdrNumRecs hdr
        numRecs        = fromIntegral recs
        (Just var)     = varByName "Speed" hdr
        layout         = recordLayout hdr (varLayout var)
        tx             = newTransformer layout

    print layout

    hSeek nc AbsoluteSeek (fromIntegral (varOffset var))
    bs <- B.hGet nc (numRecs * size layout)

    let rs      = readVector numRecs tx bs :: V.Vector Float
        xs      = V.filter (< 3.4e38) rs
        cnt     = V.length xs
        sum'    = V.sum xs
        showVec = unwords . map show . V.toList

    putStrLn $ unlines
        [ "xs  = " ++ (showVec . V.take 5) xs ++ " ..."
        , "      ... " ++ (showVec . V.drop (cnt - 5)) xs
        , "cnt = " ++ show cnt
        , "min = " ++ show (V.minimum xs)
        , "avg = " ++ show (sum' / fromIntegral cnt)
        , "max = " ++ show (V.maximum xs)
        , "sum = " ++ show sum'
        ]

    hClose nc
  where
    file = "/home/jake/data/fbf2nc/big-mbe.nc"


showVar :: Var -> String
showVar v = BC.unpack (varName v) ++ "\n" ++ unlines (map showDim (varDims v))

showDim :: Dim -> String
showDim d = "  " ++ BC.unpack (dimName d) ++ " (" ++ show (dimLength d) ++ ")"

isRecordVar :: Var -> Bool
isRecordVar = any (== Unlimited) . map dimLength . varDims

recordLayout :: Header -> Layout -> Layout
recordLayout = group . fromIntegral . recSize
  where
    recSize = sum . map (size . varLayout) . filter isRecordVar . hdrVars

varByName :: ByteString -> Header -> Maybe Var
varByName name = find ((name ==) . varName) . hdrVars

varLayout :: Var -> Layout
varLayout v = foldr go value' (varDims v)
  where
    go (Dim _ (Fixed n)) x = repeat (fromIntegral n) x
    go (Dim _ Unlimited) x = alignLayout x

    alignLayout x = if paddedSize x /= size x
                    then group (paddedSize x) x
                    else x

    paddedSize x = (size x + 3) .&. complement 0x3

    value' = case varType v of
      VarByte   -> word8
      VarChar   -> word8
      VarShort  -> word16be
      VarInt    -> word32be
      VarFloat  -> word32be
      VarDouble -> word64be

------------------------------------------------------------------------

parseHeader :: FilePath -> IO Header
parseHeader path = runResourceT (file $$ parser)
  where
    file   = sourceFile path
    parser = sinkGet getHeader

module Main where

import Data.Attoparsec.Binary
--import Data.Attoparsec.IEEE754
import Data.ByteString.Char8 (unpack)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary (sourceFile, sourceFileRange)
import Data.List
import Data.Ord
import Prelude

import Data.NetCDF.Parser (header)
import Data.NetCDF.Types

------------------------------------------------------------------------

main :: IO ()
main = do
    hd <- parseHeader file

    let vs  = hdrVars hd
        v   = ((!! 1) . filter ((== VarShort) . varType)) vs

        vs' = sortBy (comparing varOffset)
            $ filter isRecordVar vs
        v'  = varOffset (head vs')
        ws  = map (\x -> unpack (varName x) ++ ": " ++ show (varOffset x - v')) vs'

    print (hdrNumRecs hd)
    putStrLn (unlines ws)

    mapM_ (putStrLn . showVar) [v]

    print (varSize v)
    print (varOffset v)
    x <- readNumber file (varOffset v + 4 * 100)
    print x
  where
    file = "/home/jake/data/netcdf/cami_0000-09-01_64x128_L26_c030918.nc"

showVar :: Var -> String
showVar v = unpack (varName v) ++ "\n" ++ unlines (map showDim (varDims v))

showDim :: Dim -> String
showDim d = "  " ++ unpack (dimName d) ++ " (" ++ show (dimLength d) ++ ")"

isRecordVar :: Var -> Bool
isRecordVar = any (== Unlimited) . map dimLength . varDims

------------------------------------------------------------------------

parseHeader :: FilePath -> IO Header
parseHeader path = runResourceT (file $$ parser)
  where
    file   = sourceFile path
    parser = sinkParser header

readNumber :: FilePath -> FileOffset -> IO Integer
readNumber path offset = runResourceT (file $$ parser)
  where
    file   = sourceFileRange path (Just $ fromIntegral offset) Nothing
    parser = sinkParser (fromIntegral `fmap` anyWord16be)

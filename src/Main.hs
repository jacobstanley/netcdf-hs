module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Binary (sourceFile)
import Prelude hiding (take, drop)
import Text.Show.Pretty (ppShow)

import Data.NetCDF.Parser (header)

------------------------------------------------------------------------

main :: IO ()
main = runResourceT $ do
    nc <- file $$ parser
    liftIO (putStrLn $ ppShow nc)
  where
    file   = sourceFile "/home/jake/data/netcdf/sresa1b_ncar_ccsm3_0_run1_200001.nc"
    parser = sinkParser header

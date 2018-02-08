
module Main where

import           Data.Conduit
import           Data.Conduit.Binary
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion

import           Addresses
import           Instances

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

file :: FilePath
file = "./data/pat_abstracts.csv"

process :: Monad m => Conduit (Named Abstracts) m (Named Abstracts)
process = awaitForever $ yield

main :: IO ()
main = runResourceT $
  sourceFile file .|
  intoCSV (csvset ',') .|
  process .|
  fromCSV (csvset ',') $$
  sinkFile "./data/pat_output.csv"

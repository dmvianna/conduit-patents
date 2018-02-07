{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (mzero)
import qualified Data.ByteString             as B
import           Data.CaseInsensitive
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Vector                 as V
import           Text.Trifecta

import           Addresses
import           Instances

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

readCsv :: FilePath -> Char -> IO (V.Vector (Named Abstracts))
readCsv fp del = readCSVFile (csvset del) fp

file :: FilePath
file = "./data/pat_abstracts.csv"

main :: IO (V.Vector (Named Abstracts))
main = readCsv file ','


process :: Monad m => Conduit (Named Abstracts) m (Named Abstracts)
process = awaitForever $ yield

write :: IO ()
write = runResourceT $
  sourceFile file .|
  intoCSV (csvset ',') .|
  process .|
  fromCSV (csvset ',') $$
  sinkFile "./data/pat_output.csv"

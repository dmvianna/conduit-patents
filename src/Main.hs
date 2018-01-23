{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString  as B
import           Data.CSV.Conduit
import           Data.Text        (Text)
import qualified Data.Vector      as V

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

readCsv :: String -> Char -> IO (V.Vector (Row Text))
readCsv fp del = readCSVFile (csvset del) fp

main :: IO (V.Vector (Row Text))
main = readCsv "./data/pat_abstracts.csv" ','

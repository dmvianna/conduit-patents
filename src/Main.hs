{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (mzero)
import qualified Data.ByteString             as B
import           Data.CaseInsensitive
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

main :: IO (V.Vector (Named Abstracts))
main = readCsv "./data/pat_abstracts.csv" ','

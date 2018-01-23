{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString             as B
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Text                   (Text)
import qualified Data.Vector                 as V

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

readCsv :: FilePath -> Char -> IO (V.Vector (Named Abstracts))
readCsv fp del = readCSVFile (csvset del) fp

data Abstracts = Abstracts
  { _id       :: !Integer
  , _abstract :: !Text
  } deriving (Show, Eq, Read)

instance FromNamedRecord Abstracts where
  parseNamedRecord m =
    Abstracts <$>
    m .: "AUSTRALIAN_APPL_NO" <*>
    m .: "ABSTRACT_TEXT"

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _abstract) =
    namedRecord [ "id" .= _id
                , "abstract" .= _abstract
                ]

main :: IO (V.Vector (Named Abstracts))
main = readCsv "./data/pat_abstracts.csv" ','

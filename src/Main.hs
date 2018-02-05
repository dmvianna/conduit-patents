{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (mzero)
import qualified Data.ByteString.Lazy        as B
import           Data.CaseInsensitive
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Text.Trifecta

import           Addresses

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

readCsv :: FilePath -> Char -> IO (V.Vector (Named Abstracts))
readCsv fp del = readCSVFile (csvset del) fp

data Abstracts = Abstracts
  { _id       :: !Integer
  , _abstract :: !AuAddress
  } deriving (Show, Eq)

instance FromField AuAddress where
  parseField s =
    case parseByteString (step auAddress)
         mempty (foldCase s) of
      Success x -> pure x
      Failure e -> mzero

-- instance ToField AuAddress where


instance FromNamedRecord Abstracts where
  parseNamedRecord m =
    Abstracts <$>
    m .: "AUSTRALIAN_APPL_NO" <*>
    m .: "ABSTRACT_TEXT"

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _abstract) =
    namedRecord [ "id" .= _id
                -- , "abstract" .= _abstract
                ]

main :: IO (V.Vector (Named Abstracts))
main = readCsv "./data/pat_abstracts.csv" ','

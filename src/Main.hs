{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Addresses
import qualified Data.ByteString             as B
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import qualified Data.Vector                 as V
import           Text.Trifecta

csvset :: Char -> CSVSettings
csvset c = CSVSettings {csvSep = c, csvQuoteChar = Just '"'}

readCsv :: FilePath -> Char -> IO (V.Vector (Named Abstracts))
readCsv fp del = readCSVFile (csvset del) fp

data Abstracts = Abstracts
  { _id       :: !Integer
  , _abstract :: !Text
  } deriving (Show, Eq, Read)

data Patents = Patents
  { _patId   :: !Integer
  , _address :: !Text
  } deriving (Show)

instance FromNamedRecord Abstracts where
  parseNamedRecord m =
    Abstracts <$>
    m .: "AUSTRALIAN_APPL_NO" <*>
    m .: "ABSTRACT_TEXT"

addressParser :: Text -> Result AuAddress
addressParser t = parseByteString (step auAddress) mempty (
  (encodeUtf8 . T.toCaseFold) t)

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _abstract) =
    namedRecord [ "id" .= _id
                , "abstract" .= _abstract
                ]

instance ToNamedRecord Patents where
  toNamedRecord (Patents _patId _address) =
    namedRecord [ "patId" .= _patId
                , "address" .= _address -- addressParser _address
                ]

instance ToField (Result Text) where
  toField (Success x) = "success" -- encodeUtf8 $ x -- result should be ByteString
  toField (Failure e) = "failed"

main :: IO (V.Vector (Named Abstracts))
main = readCsv "./data/pat_abstracts.csv" ','

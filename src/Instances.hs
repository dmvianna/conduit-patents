{-# LANGUAGE OverloadedStrings #-}

module Instances where

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

-- | Parse in

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

instance FromNamedRecord Abstracts where
  parseNamedRecord m =
    Abstracts <$>
    m .: "AUSTRALIAN_APPL_NO" <*>
    m .: "ABSTRACT_TEXT"

-- | Parse out

-- | Address type

addressType :: AuAddress -> B.ByteString
addressType (AuAddress location locality) =
  case location of
    APobox p          -> flatPobox p
    ABag bag              -> flatBag bag
    AStreetAddress
      (StAddr nr name stype) -> "street address"

flatPobox :: Pobox -> B.ByteString
flatPobox (Gpo t) = "gpo box"
flatPobox (Po t)  = "po box"

flatBag :: Bag -> B.ByteString
flatBag (Locked t)  = "locked bag"
flatBag (Private t) = "private bag"

-- | Po box number

instance ToField Pobox where
  toField (Gpo t) = encodeUtf8 t
  toField (Po t)  = encodeUtf8 t

instance ToField Bag where
  toField (Locked t)  = encodeUtf8 t
  toField (Private t) = encodeUtf8 t

class BoxNr a where
  boxNr :: a -> B.ByteString

instance BoxNr Pobox where
  boxNr = toField

instance BoxNr Bag where
  boxNr = toField

instance BoxNr AddressLocation where
  boxNr (APobox x)         = toField x
  boxNr (ABag x)           = toField x
  boxNr (AStreetAddress _) = mempty

instance BoxNr AuAddress where
  boxNr (AuAddress x _) = boxNr x

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _abstract) =
    namedRecord [ "id" .= _id
                , "addressType" .= addressType _abstract
                , "boxNr" .= boxNr _abstract
                ]

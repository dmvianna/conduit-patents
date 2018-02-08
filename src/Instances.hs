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

-- | Locality

getLocality :: AuAddress -> Locality
getLocality (AuAddress _ x) = x

instance ToField Suburb where
  toField (Suburb x) = encodeUtf8 x

instance ToField State where
  toField (State x) = encodeUtf8 x

instance ToField Postcode where
  toField (Postcode x) = encodeUtf8 x

-- || Street Number

-- | First

-- Prefix

class FirstPrefix a where
  firstPrefix :: a -> B.ByteString

instance FirstPrefix StreetNumber where
  firstPrefix (One (Single (Prefix x) _ _))     = toField x
  firstPrefix (Range (Single (Prefix x) _ _) _) = toField x

instance FirstPrefix StreetAddress where
  firstPrefix (StAddr x _ _) = firstPrefix x

instance FirstPrefix AddressLocation where
  firstPrefix (AStreetAddress x) = firstPrefix x
  firstPrefix _                  = mempty

instance FirstPrefix AuAddress where
  firstPrefix (AuAddress x _) = firstPrefix x

-- Number

class FirstNumber a where
  firstNumber :: a -> B.ByteString

instance FirstNumber StreetNumber where
  firstNumber (One (Single _ (Number x) _))     = toField x
  firstNumber (Range (Single _ (Number x) _) _) = toField x

instance FirstNumber StreetAddress where
  firstNumber (StAddr x _ _) = firstNumber x

instance FirstNumber AddressLocation where
  firstNumber (AStreetAddress x) = firstNumber x
  firstNumber _                  = mempty

instance FirstNumber AuAddress where
  firstNumber (AuAddress x _) = firstNumber x

-- Suffix

class FirstSuffix a where
  firstSuffix :: a -> B.ByteString

instance FirstSuffix StreetNumber where
  firstSuffix (One (Single _ _ (Suffix x)))     = toField x
  firstSuffix (Range (Single _ _ (Suffix x)) _) = toField x

instance FirstSuffix StreetAddress where
  firstSuffix (StAddr x _ _) = firstSuffix x

instance FirstSuffix AddressLocation where
  firstSuffix (AStreetAddress x) = firstSuffix x
  firstSuffix _                  = mempty

instance FirstSuffix AuAddress where
  firstSuffix (AuAddress x _) = firstSuffix x

-- | Last

-- Prefix

class LastPrefix a where
  lastPrefix :: a -> B.ByteString

instance LastPrefix StreetNumber where
  lastPrefix (One _)                           = mempty
  lastPrefix (Range _ (Single (Prefix x) _ _)) = toField x

instance LastPrefix StreetAddress where
  lastPrefix (StAddr x _ _) = lastPrefix x

instance LastPrefix AddressLocation where
  lastPrefix (AStreetAddress x) = lastPrefix x
  lastPrefix _                  = mempty

instance LastPrefix AuAddress where
  lastPrefix (AuAddress x _) = lastPrefix x

-- Number

class LastNumber a where
  lastNumber :: a -> B.ByteString

instance LastNumber StreetNumber where
  lastNumber (One _)                           = mempty
  lastNumber (Range _ (Single _ (Number x) _)) = toField x

instance LastNumber StreetAddress where
  lastNumber (StAddr x _ _) = lastNumber x

instance LastNumber AddressLocation where
  lastNumber (AStreetAddress x) = lastNumber x
  lastNumber _                  = mempty

instance LastNumber AuAddress where
  lastNumber (AuAddress x _) = lastNumber x

-- Suffix

class LastSuffix a where
  lastSuffix :: a -> B.ByteString

instance LastSuffix StreetNumber where
  lastSuffix (One _)                           = mempty
  lastSuffix (Range _ (Single _ _ (Suffix x))) = toField x

instance LastSuffix StreetAddress where
  lastSuffix (StAddr x _ _) = lastSuffix x

instance LastSuffix AddressLocation where
  lastSuffix (AStreetAddress x) = lastSuffix x
  lastSuffix _                  = mempty

instance LastSuffix AuAddress where
  lastSuffix (AuAddress x _) = lastSuffix x

-- | Finalising

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _ab) =
    namedRecord [ "id" .= _id
                , "addressType" .= addressType _ab
                , "boxNr" .= boxNr _ab
                , "firstPrefix" .= firstPrefix _ab
                , "firstNumber" .= firstNumber _ab
                , "firstSuffix" .= firstSuffix _ab
                , "lastPrefix" .= lastPrefix _ab
                , "lastNumber" .= lastNumber _ab
                , "lastSuffix" .= lastSuffix _ab
                , "suburb" .= get _suburb
                , "state" .= get _state
                , "postcode" .= get _postcode
                ]
    where
      get f = toField $ f $ getLocality _ab

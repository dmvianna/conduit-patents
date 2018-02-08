{-# LANGUAGE OverloadedStrings #-}

module Instances where

import           Control.Monad               (mzero)
import qualified Data.ByteString             as B
import           Data.CaseInsensitive
import           Data.CSV.Conduit.Conversion
import           Data.Text.Encoding          (encodeUtf8)
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

--  | AddressNumbers

class AddressNumbers a where
  firstPrefix :: a -> B.ByteString
  firstNumber :: a -> B.ByteString
  firstSuffix :: a -> B.ByteString
  lastPrefix :: a -> B.ByteString
  lastNumber :: a -> B.ByteString
  lastSuffix :: a -> B.ByteString

instance AddressNumbers StreetNumber where
  firstPrefix (One (Single (Prefix x) _ _))     = toField x
  firstPrefix (Range (Single (Prefix x) _ _) _) = toField x
  firstNumber (One (Single _ (Number x) _))     = toField x
  firstNumber (Range (Single _ (Number x) _) _) = toField x
  firstSuffix (One (Single _ _ (Suffix x)))     = toField x
  firstSuffix (Range (Single _ _ (Suffix x)) _) = toField x
  lastPrefix (One _)                           = mempty
  lastPrefix (Range _ (Single (Prefix x) _ _)) = toField x
  lastNumber (One _)                           = mempty
  lastNumber (Range _ (Single _ (Number x) _)) = toField x
  lastSuffix (One _)                           = mempty
  lastSuffix (Range _ (Single _ _ (Suffix x))) = toField x

instance AddressNumbers StreetAddress where
  firstPrefix (StAddr x _ _) = firstPrefix x
  firstNumber (StAddr x _ _) = firstNumber x
  firstSuffix (StAddr x _ _) = firstSuffix x
  lastPrefix (StAddr x _ _) = lastPrefix x
  lastNumber (StAddr x _ _) = lastNumber x
  lastSuffix (StAddr x _ _) = lastSuffix x

instance AddressNumbers AddressLocation where
  firstPrefix (AStreetAddress x) = firstPrefix x
  firstPrefix _                  = mempty
  firstNumber (AStreetAddress x) = firstNumber x
  firstNumber _                  = mempty
  firstSuffix (AStreetAddress x) = firstSuffix x
  firstSuffix _                  = mempty
  lastPrefix (AStreetAddress x) = lastPrefix x
  lastPrefix _                  = mempty
  lastNumber (AStreetAddress x) = lastNumber x
  lastNumber _                  = mempty
  lastSuffix (AStreetAddress x) = lastSuffix x
  lastSuffix _                  = mempty

instance AddressNumbers AuAddress where
  firstPrefix (AuAddress x _) = firstPrefix x
  firstNumber (AuAddress x _) = firstNumber x
  firstSuffix (AuAddress x _) = firstSuffix x
  lastPrefix (AuAddress x _) = lastPrefix x
  lastNumber (AuAddress x _) = lastNumber x
  lastSuffix (AuAddress x _) = lastSuffix x

-- | StAddrFields

class StAddrFields a where
  streetName :: a -> B.ByteString
  streetType :: a -> B.ByteString

instance StAddrFields StreetAddress where
  streetName (StAddr _ (StreetName x) _) = toField x
  streetType (StAddr _ _ (StreetType x)) = toField x

instance StAddrFields AddressLocation where
  streetName (AStreetAddress x) = streetName x
  streetName _                  = mempty
  streetType (AStreetAddress x) = streetType x
  streetType _                  = mempty

instance StAddrFields AuAddress where
  streetName (AuAddress x _) = streetName x
  streetType (AuAddress x _) = streetType x

-- | Finalising

instance ToNamedRecord Abstracts where
  toNamedRecord (Abstracts _id _ab) =
    namedRecord [ "01.id" .= _id
                , "02.addressType" .= addressType _ab
                , "03.boxNr" .= boxNr _ab
                , "04.firstPrefix" .= firstPrefix _ab
                , "05.firstNumber" .= firstNumber _ab
                , "06.firstSuffix" .= firstSuffix _ab
                , "07.lastPrefix" .= lastPrefix _ab
                , "08.lastNumber" .= lastNumber _ab
                , "09.lastSuffix" .= lastSuffix _ab
                , "10.streetName" .= streetName _ab
                , "11.streetType" .= streetType _ab
                , "12.suburb" .= get _suburb
                , "13.state" .= get _state
                , "14.postcode" .= get _postcode
                ]
    where
      get f = toField $ f $ getLocality _ab

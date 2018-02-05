
module Lower where

import           Data.Array.Unboxed
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Internal as BI
import           Data.Char
import           Data.Word

ctype_lower :: UArray Word8 Word8
ctype_lower = listArray (0,255) (map (BI.c2w . toLower) ['\0'..'\255']) :: UArray Word8 Word8

lowercase :: BI.ByteString -> BI.ByteString
lowercase = B.map (\x -> ctype_lower!x)

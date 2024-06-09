module DataTypes where

import Data.Csv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

newtype Row = Row { unRow :: HM.HashMap Text Text }
  deriving (Show, Eq)

instance FromNamedRecord Row where
  parseNamedRecord m = pure $ Row $ HM.fromList [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- HM.toList m]

instance ToNamedRecord Row where
  toNamedRecord (Row m) = namedRecord [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- HM.toList m]

instance DefaultOrdered Row where
  headerOrder (Row m) = header $ map encodeUtf8 (HM.keys m)

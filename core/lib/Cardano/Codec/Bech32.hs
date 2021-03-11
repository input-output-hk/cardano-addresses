module Cardano.Codec.Bech32 (ToBech32(..), FromBech32(..), bech32With,fromBech32With) where

import Prelude

import Data.Text (Text)
import Codec.Binary.Bech32 (HumanReadablePart, humanReadablePartToText)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import qualified Codec.Binary.Encoding as E

class ToBech32 a where
  bech32 :: a -> Text

class FromBech32 a where
  fromBech32 :: Text -> Either String a

bech32With :: HumanReadablePart -> ByteString -> Text
bech32With hrp =
    T.decodeUtf8 . encode (EBech32 hrp)

fromBech32With :: HumanReadablePart -> (ByteString -> b) -> Text -> Either String b
fromBech32With hrn con text = do
  (hrp, bs) <- E.fromBech32 (const id) $ T.encodeUtf8 text
  if hrp == hrn then
    pure $ con bs
  else
    Left $ "Human Readable Part should be " <> show (humanReadablePartToText hrn) <> " but is " <> show (humanReadablePartToText hrp)
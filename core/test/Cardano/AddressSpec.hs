{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.AddressSpec
    ( spec
    ) where

import Prelude

import Cardano.Address
    ( Address
    , HasNetworkDiscriminant (..)
    , PaymentAddress (..)
    , base58
    , fromBase58
    )
import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Address.Style.Byron
    ( Byron )
import Cardano.Address.Style.Icarus
    ( Icarus )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, counterexample, label )
import Cardano.Codec.Bech32 (ToBech32 (bech32),FromBech32 (fromBech32))

import qualified Data.Text as T
import Data.Either.Extra (eitherToMaybe)

spec :: Spec
spec = describe "Text Encoding Roundtrips" $ do
    prop "base58 . fromBase58 - Byron" $
        prop_roundtripTextEncoding @Byron base58 fromBase58

    prop "base58 . fromBase58 - Icarus" $
        prop_roundtripTextEncoding @Icarus base58 fromBase58

    prop "bech32 . fromBech32 - Byron" $
        prop_roundtripTextEncoding @Byron bech32 (eitherToMaybe . fromBech32)

    prop "bech32 . fromBech32 - Icarus" $
        prop_roundtripTextEncoding @Icarus bech32 (eitherToMaybe . fromBech32)

-- Ensure that any address public key can be encoded to an address and that the
-- address can be encoded and decoded without issues.
prop_roundtripTextEncoding
    :: forall k. (PaymentAddress k)
    => (Address -> Text)
        -- ^ encode to 'Text'
    -> (Text -> Maybe Address)
        -- ^ decode from 'Text'
    -> k 'PaymentK XPub
        -- ^ An arbitrary public key
    -> NetworkDiscriminant k
        -- ^ An arbitrary network discriminant
    -> Property
prop_roundtripTextEncoding encode decode addXPub discrimination =
    (result == pure address)
        & counterexample (unlines
            [ "Address " <> T.unpack (encode address)
            , "↳       " <> maybe "ø" (T.unpack . encode) result
            ])
        & label (show $ addressDiscrimination @k discrimination)
  where
    address = paymentAddress discrimination addXPub
    result  = decode (encode address)

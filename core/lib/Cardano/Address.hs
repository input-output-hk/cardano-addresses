{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address
    ( -- * Address
      Address
    , PaymentAddress (..)
    , StakeAddress (..)
    , DelegationAddress (..)
    , PointerAddress (..)
    , ChainPointer (..)
    , unsafeMkAddress
    , unAddress

      -- * Conversion From / To Text
    , base58
    , fromBase58

      -- Internal / Network Discrimination
    , HasNetworkDiscriminant (..)
    , AddressDiscrimination (..)
    , NetworkTag (..)
    , invariantSize
    , invariantNetworkTag
    , AddressVerificationKeyHash (..)
    , StakeVerificationKeyHash (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..), XPub )
import Cardano.Codec.Cbor
    ( decodeAddress, deserialiseCbor )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (<=<) )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )

import Cardano.Codec.Bech32
    ( FromBech32 (..), ToBech32 (..), bech32With, fromBech32With )
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Codec.Binary.Encoding as E
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

-- | An 'Address' type representing 'Cardano' addresses. Internals are
-- irrevelant to the user.
--
-- @since 1.0.0
newtype Address = Address
    { unAddress :: ByteString
    } deriving stock (Generic, Show, Eq, Ord)
instance NFData Address
instance ToBech32 Address where
    bech32 (Address k) = bech32With CIP5.addr k
instance FromBech32 Address where
    fromBech32 = fromBech32With CIP5.addr Address

-- Unsafe constructor for easily lifting bytes inside an 'Address'.
--
-- /!\ Use at your own risks.
unsafeMkAddress :: ByteString -> Address
unsafeMkAddress = Address

-- | Encode an 'Address' to a base58 'Text'.
--
-- @since 1.0.0
base58 :: Address -> Text
base58 = T.decodeUtf8 . encode EBase58 . unAddress

-- | Decode a base58-encoded 'Text' into an 'Address'
--
-- @since 1.0.0
fromBase58 :: Text -> Maybe Address
fromBase58 =
    deserialiseCbor (unsafeMkAddress <$> decodeAddress)
   <=<
    eitherToMaybe . E.fromBase58 . T.encodeUtf8

-- | Encoding of addresses for certain key types and backend targets.
--
-- @since 2.0.0
class HasNetworkDiscriminant key => StakeAddress key where
    -- | Convert a delegation key to a stake 'Address' (aka: reward account address)
    -- valid for the given network discrimination.
    --
    -- @since 2.0.0
    stakeAddress :: NetworkDiscriminant key -> key 'DelegationK XPub -> Address

-- | Encoding of addresses for certain key types and backend targets.
--
-- @since 1.0.0
class HasNetworkDiscriminant key => PaymentAddress key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- @since 1.0.0
    paymentAddress :: NetworkDiscriminant key -> key 'PaymentK XPub -> Address

-- | Encoding of delegation addresses for certain key types and backend targets.
--
-- @since 2.0.0
class PaymentAddress key
    => DelegationAddress key where
    -- | Convert a public key and a delegation key to a delegation 'Address' valid
    -- for the given network discrimination. Funds sent to this address will be
    -- delegated according to the delegation settings attached to the delegation
    -- key.
    --
    -- @since 2.0.0
    delegationAddress
        :: NetworkDiscriminant key
        ->  key 'PaymentK XPub
            -- ^ Payment key
        ->  key 'DelegationK XPub
            -- ^ Delegation key
        -> Address

-- | A 'ChainPointer' type representing location of some object
-- in the blockchain (eg., delegation certificate). This can be achieved
-- unambiguously by specifying slot number, transaction index and the index
-- in the object list (eg., certification list).
-- For delegation certificates, alternatively, the delegation key can be used and
-- then 'DelegationAddress' can be used.
--
-- @since 2.0.0
data ChainPointer = ChainPointer
    { slotNum :: Natural
      -- ^ Pointer to the slot
    , transactionIndex :: Natural
      -- ^ transaction index
    , outputIndex :: Natural
      -- ^ output list index
    } deriving stock (Generic, Show, Eq, Ord)
instance NFData ChainPointer

-- | Encoding of pointer addresses for payment key type, pointer to delegation
-- certificate in the blockchain and backend targets.
--
-- @since 2.0.0
class PaymentAddress key
    => PointerAddress key where
    -- | Convert a payment public key and a pointer to delegation key in the
    -- blockchain to a delegation 'Address' valid for the given network
    -- discrimination. Funds sent to this address will be delegated according to
    -- the delegation settings attached to the delegation key located by
    -- 'ChainPointer'.
    --
    -- @since 2.0.0
    pointerAddress
        :: NetworkDiscriminant key
        ->  key 'PaymentK XPub
            -- ^ Payment key
        ->  ChainPointer
            -- ^ Pointer to locate delegation key in blockchain
        -> Address

class HasNetworkDiscriminant (key :: Depth -> * -> *) where
    type NetworkDiscriminant key :: *

    addressDiscrimination :: NetworkDiscriminant key -> AddressDiscrimination
    networkTag :: NetworkDiscriminant key -> NetworkTag

-- Magic constant associated with a given network. This is mainly used in two
-- places:
--
-- (1) In 'Address' payloads, to discriminate addresses between networks.
-- (2) At the network-level, when doing handshake with nodes.
newtype NetworkTag
    = NetworkTag { unNetworkTag :: Word32 }
    deriving (Generic, Show, Eq)
instance NFData NetworkTag

-- Describe requirements for address discrimination on the Byron era.
data AddressDiscrimination
    = RequiresNetworkTag
    | RequiresNoTag
    deriving (Generic, Show, Eq)
instance NFData AddressDiscrimination

invariantSize :: HasCallStack => Int -> ByteString -> ByteString
invariantSize expectedLength bytes
    | BS.length bytes == expectedLength = bytes
    | otherwise = error
      $ "length was "
      ++ show (BS.length bytes)
      ++ ", but expected to be "
      ++ (show expectedLength)

invariantNetworkTag :: HasCallStack => Word32 -> NetworkTag -> Word8
invariantNetworkTag limit (NetworkTag num)
    | num < limit = fromIntegral num
    | otherwise = error
      $ "network tag was "
      ++ show num
      ++ ", but expected to be less than "
      ++ show limit

newtype AddressVerificationKeyHash = AddressVerificationKeyHash { unAddressVerificationKeyHash :: ByteString }
instance ToBech32 AddressVerificationKeyHash where
    bech32 (AddressVerificationKeyHash kh) = bech32With CIP5.addr_vkh kh
instance FromBech32 AddressVerificationKeyHash where
    fromBech32 = fromBech32With CIP5.addr_vkh AddressVerificationKeyHash

newtype StakeVerificationKeyHash = StakeVerificationKeyHash { unStakeVerificationKeyHash :: ByteString }
instance ToBech32 StakeVerificationKeyHash where
    bech32 (StakeVerificationKeyHash kh) = bech32With CIP5.stake_vkh kh
instance FromBech32 StakeVerificationKeyHash where
    fromBech32 = fromBech32With CIP5.stake_vkh StakeVerificationKeyHash

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Shelley
    ( -- $overview

      -- * Shelley
      Shelley
    , getKey
    , Role (..)
    , Credential (..)
    , CredentialType (..)

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveDelegationPrivateKey
    , deriveAddressPublicKey
    , deriveMultisigForPaymentPrivateKey
    , deriveMultisigForPaymentPublicKey
    , deriveMultisigForDelegationPrivateKey
    , deriveMultisigForDelegationPublicKey
    , hashKey

      -- * Addresses
      -- $addresses
    , inspectAddress
    , inspectShelleyAddress
    , paymentAddress
    , delegationAddress
    , pointerAddress
    , stakeAddress
    , extendAddress
    , ErrExtendAddress (..)
    , ErrInspectAddress (..)
    , prettyErrInspectAddress

      -- * Network Discrimination
    , MkNetworkDiscriminantError (..)
    , mkNetworkDiscriminant
    , inspectNetworkDiscriminant
    , shelleyMainnet
    , shelleyTestnet

      -- * Unsafe
    , liftXPrv
    , liftXPub
    , unsafeFromRight

      -- Internals
    , minSeedLengthBytes
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , ChainPointer (..)
    , NetworkDiscriminant (..)
    , NetworkTag (..)
    , invariantNetworkTag
    , invariantSize
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index
    , hardenedIndex
    , softIndex
    , XPrv
    , XPub
    , credentialHashSize
    , deriveXPrv
    , deriveXPub
    , generateNew
    , hashCredential
    , xpubPublicKey
    )
import Cardano.Address.Script
    ( KeyHash (..), ScriptHash (..) )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( Alternative, (<|>) )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( Exception, displayException )
import Control.Exception.Base
    ( assert )
import Control.Monad
    ( guard, unless, when )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Data.Aeson
    ( (.=) )
import Data.Binary.Get
    ( runGetOrFail )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( (.&.) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.Maybe
    ( fromMaybe, isNothing )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import Data.Word7
    ( getVariableLengthNat, putVariableLengthNat )
import Fmt
    ( Buildable, build, format, (+|), (|+) )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address.Derivation as Internal
import qualified Cardano.Address.Style.Byron as Byron
import qualified Cardano.Address.Style.Icarus as Icarus
import qualified Data.Aeson as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Shelley master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
--
-- - 'paymentAddress': for constructing payment addresses from a address public key or a script
-- - 'delegationAddress': for constructing delegation addresses from payment credential (public key or script) and stake credential (public key or script)
-- - 'pointerAddress': for constructing delegation addresses from payment credential (public key or script) and chain pointer
-- - 'stakeAddress': for constructing reward accounts from stake credential (public key or script)

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Shelley 'RootK XPrv
-- let accountPubKey  = Shelley 'AccountK XPub
-- let addressPubKey  = Shelley 'PaymentK XPub
-- @
--
-- @since 2.0.0
newtype Shelley (depth :: Depth) key = Shelley
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Shelley depth))
instance (NFData key) => NFData (Shelley depth key)

-- | Describe what the keys within an account are used for.
--
-- - UTxOExternal: used for public addresses sent to other parties for receiving money.
-- - UTxOInternal: generated by wallet software to send change back to the wallet.
-- - Stake: used for stake key(s) and delegation.
-- - MultisigForPayment: used for keys used in multi-party transactions / scripts in payment credential.
-- - MultisigForDelegation: used for keys used in multi-party transactions / scripts in delegation credential.
--
-- @since 3.0.0
data Role
    = UTxOExternal
    | UTxOInternal
    | Stake
    | MultisigForPayment
    | MultisigForDelegation
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData Role

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).

-- XXX we should probably remove this instance
instance Enum Role where
    toEnum = \case
        0 -> UTxOExternal
        1 -> UTxOInternal
        2 -> Stake
        3 -> MultisigForPayment
        4 -> MultisigForDelegation
        _ -> error "Role.toEnum: bad argument"
    fromEnum = \case
        UTxOExternal -> 0
        UTxOInternal -> 1
        Stake -> 2
        MultisigForPayment -> 3
        MultisigForDelegation -> 4

roleIndex :: Role -> Word32
roleIndex UTxOExternal = 0
roleIndex UTxOInternal = 1
roleIndex Stake = 2
roleIndex MultisigForPayment = 3
roleIndex MultisigForDelegation = 4

--
-- Key Derivation
--
-- $keyDerivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Shelley 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveDelegationPrivateKey acctK

instance Internal.GenMasterKey Shelley where
    type SecondFactor Shelley = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Shelley $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance Internal.HardDerivation Shelley where
    type AccountIndexDerivationType Shelley = 'Hardened
    type AddressIndexDerivationType Shelley = 'Soft
    type WithRole Shelley = Role

    deriveAccountPrivateKey (Shelley rootXPrv) accIx =
        let
            Just purposeIx = hardenedIndex purposeIndex
            Just coinTypeIx = hardenedIndex coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Shelley acctXPrv

    deriveAddressPrivateKey (Shelley accXPrv) role addrIx =
        let
            Just roleCode = softIndex (roleIndex role)
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv roleCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Shelley addrXPrv

instance Internal.SoftDerivation Shelley where
    deriveAddressPublicKey (Shelley accXPub) role addrIx =
        fromMaybe errWrongIndex $ do
            roleCode <- softIndex (roleIndex role)
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub roleCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Shelley addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 2.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Shelley 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 2.0.0
genMasterKeyFromXPrv
    :: XPrv -> Shelley 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 2.0.0
deriveAccountPrivateKey
    :: Shelley 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Shelley 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 2.0.0
deriveAddressPrivateKey
    :: Shelley 'AccountK XPrv
    -> Role
    -> Index 'Soft 'PaymentK
    -> Shelley 'PaymentK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 2.0.0
deriveAddressPublicKey
    :: Shelley 'AccountK XPub
    -> Role
    -> Index 'Soft 'PaymentK
    -> Shelley 'PaymentK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derive a delegation key for a corresponding 'AccountK'. Note that wallet
-- software are by convention only using one delegation key per account, and always
-- the first account (with index 0').
--
-- Deriving delegation keys for something else than the initial account is not
-- recommended and can lead to incompatibility with existing wallet softwares
-- (Daedalus, Yoroi, Adalite...).
--
-- @since 2.0.0
deriveDelegationPrivateKey
    :: Shelley 'AccountK XPrv
    -> Shelley 'DelegationK XPrv
deriveDelegationPrivateKey accXPrv =
    let (Shelley stakeXPrv) =
            deriveAddressPrivateKey accXPrv Stake (minBound @(Index 'Soft _))
    in Shelley stakeXPrv


-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives a multisig private key from the given account private key for payment credential.
--
-- @since 3.0.0
deriveMultisigForPaymentPrivateKey
    :: Shelley 'AccountK XPrv
    -> Index 'Soft 'PaymentK
    -> Shelley 'ScriptK XPrv
deriveMultisigForPaymentPrivateKey accPrv addrIx =
    let (Shelley xprv) = Internal.deriveAddressPrivateKey accPrv MultisigForPayment addrIx
    in Shelley xprv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives a multisig private key from the given account private key for delegation credential.
--
-- @since 3.3.0
deriveMultisigForDelegationPrivateKey
    :: Shelley 'AccountK XPrv
    -> Index 'Soft 'PaymentK
    -> Shelley 'ScriptK XPrv
deriveMultisigForDelegationPrivateKey accPrv addrIx =
    let (Shelley xprv) = Internal.deriveAddressPrivateKey accPrv MultisigForDelegation addrIx
    in Shelley xprv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives a multisig public key from the given account public key for payment credential.
--
-- @since 3.0.0
deriveMultisigForPaymentPublicKey
    :: Shelley 'AccountK XPub
    -> Index 'Soft 'PaymentK
    -> Shelley 'ScriptK XPub
deriveMultisigForPaymentPublicKey accPub addrIx =
    let (Shelley xpub) = Internal.deriveAddressPublicKey accPub MultisigForPayment addrIx
    in Shelley xpub

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives a multisig public key from the given account public key for delegation credential.
--
-- @since 3.3.0
deriveMultisigForDelegationPublicKey
    :: Shelley 'AccountK XPub
    -> Index 'Soft 'PaymentK
    -> Shelley 'ScriptK XPub
deriveMultisigForDelegationPublicKey accPub addrIx =
    let (Shelley xpub) = Internal.deriveAddressPublicKey accPub MultisigForDelegation addrIx
    in Shelley xpub

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress' from public key credential
--
-- > import Cardano.Address ( bech32 )
-- > import Cardano.Address.Derivation ( toXPub )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > bech32 $ paymentAddress tag paymentCredential
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
--
-- === Generating a 'PaymentAddress' from script credential
--
-- > import Cardano.Address.Script.Parser ( scriptFromString )
-- > import Cardano.Address.Script ( toScriptHash )
-- > import Codec.Binary.Encoding ( encode )
-- > import Data.Text.Encoding ( decodeUtf8 )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let verKey1 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyreluzt36ms"
-- > let verKey2 = "script_vkh18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrenxv223vj"
-- > let scriptStr = "all [" ++ verKey1 ++ ", " ++ verKey2 ++ "]"
-- > let (Right script) = scriptFromString scriptStr
-- > let scriptHash@(ScriptHash bytes) = toScriptHash script
-- > decodeUtf8 (encode EBase16 bytes)
-- > "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
-- > bech32 $ paymentAddress tag (PaymentFromScript scriptHash)
-- > "addr1wxspttnpqa0zts7ey59ae0p4ce2hyusj0yn7eu4z6utw98c9uxm83"
--
-- === Generating a 'DelegationAddress'
--
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > let delegationCredential = DelegationFromKey $ (toXPub <$> stakeK)
-- > bech32 $ delegationAddress tag paymentCredential delegationCredential
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"
--
-- === Generating a 'PointerAddress'
--
-- > import Cardano.Address ( ChainPointer (..) )
-- >
-- > let (Right tag) = mkNetworkDiscriminant 1
-- > let ptr = ChainPointer 123 1 2
-- > let paymentCredential = PaymentFromKey $ (toXPub <$> addrK)
-- > bech32 $ pointerAddress tag paymentCredential ptr
-- > "addr1gxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdnmmqypqfcp5um"
--
-- === Generating a 'DelegationAddress' from using the same script credential in both payment and delegation
-- > bech32 $ delegationAddress tag (PaymentFromScript scriptHash) (DelegationFromScript scriptHash)
-- > "addr1xxspttnpqa0zts7ey59ae0p4ce2hyusj0yn7eu4z6utw98aqzkhxzp67yhpajfgtmj7rt3j4wfepy7f8ane294cku20swucnrl"


-- | Possible errors from inspecting a Shelley address
--
-- @since 3.0.0
data ErrInspectAddress
    = UnknownAddrType
    | WrongInputSize Int -- ^ Actual size
    | PtrRetrieveError String -- ^ Human readable error of underlying operation
    deriving (Eq, Show)

instance Exception ErrInspectAddress where
    displayException = prettyErrInspectAddress

-- | Pretty-print an 'ErrInspectAddress'
--
-- @since 3.0.0
prettyErrInspectAddress :: ErrInspectAddress -> String
prettyErrInspectAddress = \case
    UnknownAddrType ->
        "Unknown address type"
    WrongInputSize i ->
        format "Wrong input size of {}" i
    PtrRetrieveError s ->
        format "Failed to retrieve pointer (underlying errors was: {})" s

-- Analyze an 'Address' to know whether it's a Shelley address or not.
--
-- Throws 'AddrError' if it's not a valid Shelley address, or a ready-to-print
-- string giving details about the 'Address'.
--
-- @since 2.0.0
inspectShelleyAddress
    :: (Alternative m, MonadThrow m)
    => Maybe XPub
    -> Address
    -> m Json.Value
inspectShelleyAddress = inspectAddress
{-# DEPRECATED inspectShelleyAddress "use qualified 'inspectAddress' instead." #-}

-- | Analyze an 'Address' to know whether it's a Shelley address or not.
--
-- Throws 'AddrError' if it's not a valid Shelley address, or a ready-to-print
-- string giving details about the 'Address'.
--
-- @since 3.0.0
inspectAddress
    :: (Alternative m, MonadThrow m)
    => Maybe XPub
    -> Address
    -> m Json.Value
inspectAddress mRootPub addr
    | BS.length bytes < 1 + credentialHashSize = throwM (WrongInputSize (BS.length bytes))
    | otherwise =
        let
            (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes
            addrType = fstByte .&. 0b11110000
            network  = fstByte .&. 0b00001111
        in
            case addrType of
               -- 0000: base address: keyhash28,keyhash28
                0b00000000 | BS.length rest == 2 * credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "spending_key_hash" .= base16 (BS.take credentialHashSize rest)
                        , "stake_key_hash"    .= base16 (BS.drop credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 0001: base address: scripthash32,keyhash28
                0b00010000 | BS.length rest == credentialHashSize + credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "script_hash"       .= base16 (BS.take credentialHashSize rest)
                        , "stake_key_hash"    .= base16 (BS.drop credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 0010: base address: keyhash28,scripthash32
                0b00100000 | BS.length rest == credentialHashSize + credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "spending_key_hash" .= base16 (BS.take credentialHashSize rest)
                        , "stake_script_hash" .= base16 (BS.drop credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 0011: base address: scripthash32,scripthash32
                0b00110000 | BS.length rest == 2 * credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "script_hash"       .= base16 (BS.take credentialHashSize rest)
                        , "stake_script_hash" .= base16 (BS.drop credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 0100: pointer address: keyhash28, 3 variable length uint
               -- TODO Could fo something better for pointer and try decoding
               --      the pointer
                0b01000000 | BS.length rest > credentialHashSize -> do
                    ptr <- getPtr (BS.drop credentialHashSize rest)
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by pointer"
                        , "spending_key_hash" .= base16 (BS.take credentialHashSize rest)
                        , "pointer"           .= ptrToJSON ptr
                        , "network_tag"       .= network
                        ]
               -- 0101: pointer address: scripthash32, 3 variable length uint
               -- TODO Could fo something better for pointer and try decoding
               --      the pointer
                0b01010000 | BS.length rest > credentialHashSize -> do
                    ptr <- getPtr (BS.drop credentialHashSize rest)
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by pointer"
                        , "script_hash"       .= base16 (BS.take credentialHashSize rest)
                        , "pointer"           .= ptrToJSON ptr
                        , "network_tag"       .= network
                        ]
               -- 0110: enterprise address: keyhash28
                0b01100000 | BS.length rest == credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "none"
                        , "spending_key_hash" .= base16 (BS.take credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 0111: enterprise address: scripthash32
                0b01110000 | BS.length rest == credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "none"
                        , "script_hash"       .= base16 (BS.take credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 1000: byron address
                0b10000000 ->
                    Icarus.inspectAddress addr <|> Byron.inspectAddress mRootPub addr
               -- 1110: reward account: keyhash28
                0b11100000 | BS.length rest == credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "stake_key_hash"    .= base16 (BS.take credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
               -- 1111: reward account: scripthash32
                0b11110000 | BS.length rest == credentialHashSize ->
                    pure $ Json.object
                        [ "address_style"     .= Json.String "Shelley"
                        , "stake_reference"   .= Json.String "by value"
                        , "script_hash"       .= base16 (BS.take credentialHashSize rest)
                        , "network_tag"       .= network
                        ]
                _ -> throwM UnknownAddrType
  where
    bytes  = unAddress addr
    base16 = T.unpack . T.decodeUtf8 . encode EBase16

    ptrToJSON :: ChainPointer -> Json.Value
    ptrToJSON ChainPointer{slotNum,transactionIndex,outputIndex} = Json.object
        [ "slot_num" .= slotNum
        , "transaction_index" .= transactionIndex
        , "output_index" .= outputIndex
        ]

    getPtr :: (Alternative m, MonadThrow m) => ByteString -> m ChainPointer
    getPtr source = case runGetOrFail get (BL.fromStrict source) of
        Left (_, _, e) -> throwM (PtrRetrieveError e)
        Right (rest, _, a) -> guard (BL.null rest) $> a
      where
        get = ChainPointer
            <$> getVariableLengthNat
            <*> getVariableLengthNat
            <*> getVariableLengthNat

-- | Shelley offers several ways to identify ownership of entities on chain.
--
-- This data-family has two instances, depending on whether the key is used for
-- payment or for delegation.
--
-- @since 3.0.0
data family Credential (purpose :: Depth)

data instance Credential 'PaymentK where
    PaymentFromKey :: Shelley 'PaymentK XPub -> Credential 'PaymentK
    PaymentFromScript :: ScriptHash -> Credential 'PaymentK
    deriving Show

data instance Credential 'DelegationK where
    DelegationFromKey :: Shelley 'DelegationK XPub -> Credential 'DelegationK
    DelegationFromScript :: ScriptHash -> Credential 'DelegationK
    DelegationFromPointer :: ChainPointer -> Credential 'DelegationK
    deriving Show

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a payment credential (key or script) to a payment 'Address' valid
-- for the given network discrimination.
--
-- @since 2.0.0
paymentAddress
    :: NetworkDiscriminant Shelley
    -> Credential 'PaymentK
    -> Address
paymentAddress discrimination = \case
    PaymentFromKey keyPub ->
        constructPayload
            (EnterpriseAddress CredentialFromKey)
            discrimination
            (hashCredential . xpubPublicKey . getKey $ keyPub)
    PaymentFromScript (ScriptHash bytes) ->
        constructPayload
            (EnterpriseAddress CredentialFromScript)
            discrimination
            bytes

-- | Convert a payment credential (key or script) and a delegation credential (key or script)
-- to a delegation 'Address' valid for the given network discrimination.
-- Funds sent to this address will be delegated according to the delegation settings
-- attached to the delegation key.
--
-- @since 2.0.0
delegationAddress
    :: NetworkDiscriminant Shelley
    -> Credential 'PaymentK
    -> Credential 'DelegationK
    -> Address
delegationAddress discrimination paymentCredential stakeCredential =
    unsafeFromRight $ extendAddress
        (paymentAddress discrimination paymentCredential)
        stakeCredential

-- | Convert a payment credential (key or script) and pointer to delegation certificate in blockchain to a
-- pointer 'Address' valid for the given network discrimination.
--
-- @since 3.0.0
pointerAddress
    :: NetworkDiscriminant Shelley
    -> Credential 'PaymentK
    -> ChainPointer
    -> Address
pointerAddress discrimination credential pointer =
    unsafeFromRight $ extendAddress
        (paymentAddress discrimination credential)
        (DelegationFromPointer pointer)

-- | Convert a delegation credential (key or script) to a stake Address (aka reward account address)
-- for the given network discrimination.
--
-- @since 3.0.0
stakeAddress
    :: NetworkDiscriminant Shelley
    -> Credential 'DelegationK
    -> Either ErrInvalidStakeAddress Address
stakeAddress discrimination = \case
    DelegationFromKey keyPub ->
        Right $ constructPayload
            (RewardAccount CredentialFromKey)
            discrimination
            (hashCredential . xpubPublicKey . getKey $ keyPub)

    DelegationFromScript (ScriptHash bytes) ->
        Right $ constructPayload
            (RewardAccount CredentialFromScript)
            discrimination
            bytes

    DelegationFromPointer{} ->
        Left ErrStakeAddressFromPointer

-- | Stake addresses can only be constructed from key or script hash. Trying to
-- create one from a pointer will result in the following error.
--
-- @since 3.0.0
data ErrInvalidStakeAddress
    = ErrStakeAddressFromPointer
    deriving (Generic, Show, Eq)

-- | Extend an existing payment 'Address' to make it a delegation address.
--
-- @since 2.0.0
extendAddress
    :: Address
    -> Credential 'DelegationK
    -> Either ErrExtendAddress Address
extendAddress addr stakeReference = do
    when (isNothing (inspectShelleyAddress Nothing addr)) $
        Left $ ErrInvalidAddressStyle "Given address isn't a Shelley address"

    let bytes = unAddress addr
    let (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes

    let paymentFirstByte = fstByte .&. 0b11110000
    let extendableTypes = addressType <$>
            [ EnterpriseAddress CredentialFromKey
            , EnterpriseAddress CredentialFromScript
            ]
    unless (paymentFirstByte `elem` extendableTypes) $ do
        Left $ ErrInvalidAddressType "Only payment addresses can be extended"

    case stakeReference of
        -- base address: keyhash28,keyhash28    : 00000000 -> 0
        -- base address: scripthash32,keyhash28 : 00010000 -> 16
        DelegationFromKey delegationKey -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b00011111 = 0
                -- 0b01110000 .&. 0b00011111 = 16
                putWord8 $ fstByte .&. 0b00011111
                putByteString rest
                putByteString . hashCredential . xpubPublicKey . getKey $ delegationKey

        -- base address: keyhash28,scripthash32    : 00100000 -> 32
        -- base address: scripthash32,scripthash32 : 00110000 -> 48
        DelegationFromScript (ScriptHash scriptBytes) -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b00111111 = 32
                -- 0b01110000 .&. 0b00111111 = 48
                putWord8 $ fstByte .&. 0b00111111
                putByteString rest
                putByteString scriptBytes

        -- pointer address: keyhash28, 3 variable length uint    : 01000000 -> 64
        -- pointer address: scripthash32, 3 variable length uint : 01010000 -> 80
        DelegationFromPointer pointer -> do
            pure $ unsafeMkAddress $ BL.toStrict $ runPut $ do
                -- 0b01100000 .&. 0b01011111 = 64
                -- 0b01110000 .&. 0b01011111 = 80
                putWord8 $ fstByte .&. 0b01011111
                putByteString rest
                putPointer pointer
  where
    putPointer (ChainPointer a b c) = do
        putVariableLengthNat a
        putVariableLengthNat b
        putVariableLengthNat c

-- | Captures error occuring when trying to extend an invalid address.
--
-- @since 2.0.0
data ErrExtendAddress
    = ErrInvalidAddressStyle String
    | ErrInvalidAddressType String
    deriving (Show)

--
-- Network Discriminant
--

instance HasNetworkDiscriminant Shelley where
    type NetworkDiscriminant Shelley = NetworkTag
    addressDiscrimination _ = RequiresNetworkTag
    networkTag = id

-- | Error reported from trying to create a network discriminant from number
--
-- @since 2.0.0
newtype MkNetworkDiscriminantError
    = ErrWrongNetworkTag Integer
      -- ^ Wrong network tag.
    deriving (Eq, Show)

instance Buildable MkNetworkDiscriminantError where
  build (ErrWrongNetworkTag i) = "Invalid network tag "+|i|+". Must be between [0, 15]"

-- | Construct 'NetworkDiscriminant' for Cardano 'Shelley' from a number.
-- If the number is invalid, ie., not between 0 and 15, then
-- 'MkNetworkDiscriminantError' is thrown.
--
-- @since 2.0.0
mkNetworkDiscriminant
    :: Integer
    -> Either MkNetworkDiscriminantError (NetworkDiscriminant Shelley)
mkNetworkDiscriminant nTag
    | nTag < 16 =  Right $ NetworkTag $ fromIntegral nTag
    | otherwise = Left $ ErrWrongNetworkTag nTag

-- | Retrieve the network discriminant of a given 'Address'.
-- If the 'Address' is malformed or, not a shelley address, returns Nothing.
--
-- @since 2.0.0
inspectNetworkDiscriminant
    :: Address
    -> Maybe (NetworkDiscriminant Shelley)
inspectNetworkDiscriminant addr =
    inspectShelleyAddress Nothing addr $>
        let
            bytes = unAddress addr
            (fstByte, _) = first BS.head $ BS.splitAt 1 bytes
            network  = fstByte .&. 0b00001111
        in
            NetworkTag $ fromIntegral network

-- | 'NetworkDicriminant' for Cardano MainNet & Shelley
--
-- @since 2.0.0
shelleyMainnet :: NetworkDiscriminant Shelley
shelleyMainnet = NetworkTag 1

-- | 'NetworkDicriminant' for Cardano Testnet & Shelley
--
-- @since 2.0.0
shelleyTestnet :: NetworkDiscriminant Shelley
shelleyTestnet = NetworkTag 0

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Shelley depth XPrv
liftXPrv = Shelley

-- | Unsafe backdoor for constructing an 'Shelley' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPub :: XPub -> Shelley depth XPub
liftXPub = Shelley

-- Use with care when it is _safe_.
unsafeFromRight :: Either a c -> c
unsafeFromRight =
    either (error "impossible: internally generated invalid address") id

--
-- Internal
--

--- | Computes a 28-byte Blake2b224 digest of a Shelley 'XPub'.
---
--- @since 3.0.0
hashKey :: Shelley key XPub -> KeyHash
hashKey = KeyHash . hashCredential . xpubPublicKey . getKey

-- Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

-- One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- The minimum seed length for 'genMasterKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- A sum-type for constructing addresses payment part.
data CredentialType = CredentialFromKey | CredentialFromScript
    deriving (Show, Eq)

-- Different types of Shelley addresses.
data AddressType
    = BaseAddress CredentialType CredentialType
    | PointerAddress CredentialType
    | EnterpriseAddress CredentialType
    | RewardAccount CredentialType
    | ByronAddress
    deriving (Show, Eq)

addressType :: AddressType -> Word8
addressType = \case
    ByronAddress                                                -> 0b10000000
    BaseAddress       CredentialFromKey    CredentialFromKey    -> 0b00000000
    BaseAddress       CredentialFromScript CredentialFromKey    -> 0b00010000
    BaseAddress       CredentialFromKey    CredentialFromScript -> 0b00100000
    BaseAddress       CredentialFromScript CredentialFromScript -> 0b00110000
    PointerAddress    CredentialFromKey                         -> 0b01000000
    PointerAddress    CredentialFromScript                      -> 0b01010000
    EnterpriseAddress CredentialFromKey                         -> 0b01100000
    EnterpriseAddress CredentialFromScript                      -> 0b01110000
    RewardAccount                          CredentialFromKey    -> 0b11100000
    RewardAccount                          CredentialFromScript -> 0b11110000

-- Helper to constructs appropriate address headers. Rest of the payload is left
-- to the caller as a raw 'ByteString'.
constructPayload
    :: AddressType
    -> NetworkDiscriminant Shelley
    -> ByteString
    -> Address
constructPayload addrType discrimination bytes = unsafeMkAddress $
    invariantSize expectedLength $ BL.toStrict $ runPut $ do
        putWord8 firstByte
        putByteString bytes
  where
    firstByte =
        let netTagLimit = 16
        in addressType addrType + invariantNetworkTag netTagLimit (networkTag @Shelley discrimination)
    expectedLength =
        let headerSizeBytes = 1
        in headerSizeBytes + credentialHashSize

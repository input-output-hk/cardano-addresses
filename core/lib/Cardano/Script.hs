{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Script
    (
    -- * Script
      Script (..)
    , validateScript
    , ErrValidateScript (..)
    , prettyErrValidateScript

    -- * Hashing
    , serialize

    , ScriptHash (..)
    , scriptHashFromBytes
    , toScriptHash

    , KeyHash (..)
    , keyHashFromBytes

    -- * Internal
    , hashSize
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( when )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( foldl', traverse_ )
import Data.Maybe
    ( isNothing )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L

-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script
    = RequireSignatureOf !KeyHash
    | RequireAllOf ![Script]
    | RequireAnyOf ![Script]
    | RequireMOf Word8 ![Script]
    deriving stock (Generic, Show, Eq)
instance NFData Script

-- | Validate 'Script'
--
-- @since 3.0.0
validateScript :: Script -> Either ErrValidateScript ()
validateScript = \case
    RequireSignatureOf (KeyHash bytes) -> do
        when (isNothing (keyHashFromBytes bytes)) $ Left WrongKeyHash

    RequireAllOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireAnyOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireMOf m script -> do
        when (m == 0) $ Left MZero
        when (length script < fromIntegral m) $ Left ListTooSmall
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script
  where
    hasDuplicate xs = do
        length sigs /= length (L.nub sigs)
      where
        sigs = [ sig | RequireSignatureOf sig <- xs ]

-- | Possible validation errors when validating a script
data ErrValidateScript
    = EmptyList
    | ListTooSmall
    | MZero
    | DuplicateSignatures
    | WrongKeyHash
    deriving (Eq, Show)

prettyErrValidateScript
    :: ErrValidateScript
    -> String
prettyErrValidateScript = \case
    EmptyList ->
        "The list inside a script is empty."
    MZero ->
        "The M in at_least cannot be 0."
    ListTooSmall ->
        "The list inside at_least cannot be less than M."
    DuplicateSignatures ->
        "The list inside a script has duplicate keys."
    WrongKeyHash ->
        "The hash of verification key is expected to have "<>show hashSize<>" bytes."

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
serialize :: Script -> ByteString
serialize script =
    multisigTag <> CBOR.toStrictByteString (toCBOR script)
  where
    -- | Magic number representing the tag of the native multi-signature script
    -- language. For each script language included, a new tag is chosen.
    multisigTag :: ByteString
    multisigTag = "\00"

    toCBOR :: Script -> CBOR.Encoding
    toCBOR = \case
        RequireSignatureOf (KeyHash verKeyHash) ->
            encodeMultiscriptCtr 0 2 <> CBOR.encodeBytes verKeyHash
        RequireAllOf contents ->
            encodeMultiscriptCtr 1 2 <> encodeFoldable toCBOR contents
        RequireAnyOf contents ->
            encodeMultiscriptCtr 2 2 <> encodeFoldable toCBOR contents
        RequireMOf m contents -> mconcat
            [ encodeMultiscriptCtr 3 3
            , CBOR.encodeInt (fromInteger $ toInteger m)
            , encodeFoldable toCBOR contents
            ]

    encodeMultiscriptCtr :: Word -> Word -> CBOR.Encoding
    encodeMultiscriptCtr ctrIndex listLen =
        CBOR.encodeListLen listLen <> CBOR.encodeWord ctrIndex

    encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
    encodeFoldable encode xs = wrapArray len contents
      where
        (len, contents) = foldl' go (0, mempty) xs
        go (!l, !enc) next = (l + 1, enc <> encode next)

        wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
        wrapArray len' contents'
            | len' <= 23 = CBOR.encodeListLen len' <> contents'
            | otherwise  = CBOR.encodeListLenIndef <> contents' <> CBOR.encodeBreak

-- | Computes the hash of a given script, by first serializing it to CBOR.
toScriptHash :: Script -> ScriptHash
toScriptHash = ScriptHash . blake2b224 . serialize

-- | A 'ScriptHash' type represents script hash. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype ScriptHash = ScriptHash ByteString
    deriving (Generic, Show, Eq)
instance NFData ScriptHash

-- | Construct an 'ScriptHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
scriptHashFromBytes :: ByteString -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes /= hashSize = Nothing
    | otherwise = Just $ ScriptHash bytes

-- | A 'KeyHash' type represents verification key hash that participate in building
-- multi-signature script. The hash is expected to have size of 28-byte.
--
-- @since 3.0.0
newtype KeyHash = KeyHash ByteString
    deriving (Generic, Show, Eq)
instance NFData KeyHash

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: ByteString -> Maybe KeyHash
keyHashFromBytes bytes
    | BS.length bytes /= hashSize = Nothing
    | otherwise = Just $ KeyHash bytes

--
-- Internal
--

-- Hash a public key
blake2b224 :: ByteString -> ByteString
blake2b224 =
    BA.convert . hash @_ @Blake2b_224

-- Size, in bytes, of a hash of public key (without the corresponding chain code)
hashSize :: Int
hashSize = hashDigestSize Blake2b_224
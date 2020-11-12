{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.Script
    (
    -- * Script
      Script (..)
    , serialize
    , bech32

    -- * Hashing
    , ScriptHash (..)
    , toScriptHash
    , scriptHashFromBytes

    , KeyHash (..)
    , keyHashFromBytes
    , keyHashFromText
    , ErrKeyHashFromText
    , prettyErrKeyHashFromText
    ) where

import Prelude

import Cardano.Address.Derivation
    ( credentialHashSize, hashCredential )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.=)
    )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Foldable
    ( foldl' )
import Data.Functor
    ( ($>) )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T


-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script
    = RequireSignatureOf !KeyHash
    | RequireAllOf ![Script]
    | RequireAnyOf ![Script]
    | RequireSomeOf Word8 ![Script]
    deriving stock (Generic, Show, Eq)
instance NFData Script

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
--
-- @since 3.0.0
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
        RequireSomeOf m contents -> mconcat
            [ encodeMultiscriptCtr 3 3
            , CBOR.encodeInt (fromInteger $ toInteger m)
            , encodeFoldable toCBOR contents
            ]

    encodeMultiscriptCtr :: Word -> Word -> CBOR.Encoding
    encodeMultiscriptCtr ctrIndex listLen =
        CBOR.encodeListLen listLen <> CBOR.encodeWord ctrIndex

    encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
    encodeFoldable encode' xs = wrapArray len contents
      where
        (len, contents) = foldl' go (0, mempty) xs
        go (!l, !enc) next = (l + 1, enc <> encode' next)

        wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
        wrapArray len' contents'
            | len' <= 23 = CBOR.encodeListLen len' <> contents'
            | otherwise  = CBOR.encodeListLenIndef <> contents' <> CBOR.encodeBreak

-- | Computes the hash of a given script, by first serializing it to CBOR.
--
-- @since 3.0.0
toScriptHash :: Script -> ScriptHash
toScriptHash = ScriptHash . hashCredential . serialize

-- | A 'ScriptHash' type represents script hash. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
    deriving (Generic, Show, Eq)
instance NFData ScriptHash

-- | Construct an 'ScriptHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
scriptHashFromBytes :: ByteString -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ ScriptHash bytes

-- | A 'KeyHash' type represents verification key hash that participate in building
-- multi-signature script. The hash is expected to have size of 28-byte.
--
-- @since 3.0.0
newtype KeyHash = KeyHash { unKeyHash :: ByteString }
    deriving (Generic, Show, Eq)
instance NFData KeyHash

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: ByteString -> Maybe KeyHash
keyHashFromBytes bytes
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ KeyHash bytes

-- | Construct a 'KeyHash' from 'Text'. Either hex encoded text or
-- Bech32 encoded text with `script_vkh` hrp is expected. Also
-- binary payload is expected to be composed of 28 bytes.
--
-- @since 3.0.0
keyHashFromText :: Text -> Either ErrKeyHashFromText KeyHash
keyHashFromText txt = do
    (hrp, dp) <- either
        (const $ Left ErrKeyHashFromTextInvalidString)
        Right
        (Bech32.decodeLenient txt)

    (guardHrp hrp $> dp)
        >>= maybeToRight ErrKeyHashFromTextWrongDataPart . Bech32.dataPartToBytes
        >>= maybeToRight ErrKeyHashFromTextWrongPayload . keyHashFromBytes
 where
    guardHrp hrp
        | hrp == CIP5.script_vkh = Right ()
        | otherwise = Left ErrKeyHashFromTextWrongHrp

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
data ErrKeyHashFromText
    = ErrKeyHashFromTextInvalidString
    | ErrKeyHashFromTextWrongPayload
    | ErrKeyHashFromTextWrongHrp
    | ErrKeyHashFromTextWrongDataPart
    deriving (Show, Eq)

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
prettyErrKeyHashFromText :: ErrKeyHashFromText -> String
prettyErrKeyHashFromText = \case
    ErrKeyHashFromTextInvalidString ->
        "Invalid encoded string: must be bech32-encoded."
    ErrKeyHashFromTextWrongPayload ->
        "Verification key hash must contain exactly 28 bytes."
    ErrKeyHashFromTextWrongHrp ->
        "Invalid human-readable prefix: must be 'script_vkh'."
    ErrKeyHashFromTextWrongDataPart ->
        "Verification key hash is Bech32-encoded but has an invalid data part."

--
-- Internal
--

-- | Encode a 'KeyHash' to bech32 'Text', using @script_vkh@ as a human readable prefix.
--
-- @since 3.0.0
bech32 :: KeyHash -> Text
bech32 (KeyHash keyHash) =
    T.decodeUtf8 $ encode (EBech32 CIP5.script_vkh) keyHash

-- Examples of Script jsons:
--"e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--          ]
--}
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , {"any": [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--                    , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735c"
--                    ]
--            }
--          ]
--}
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , {"some": { "from" :[ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--                               , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735c"
--                               , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735d"
--                               ]
--                     , "at_least" : 2
--                     }
--            }
--          ]
--}

instance ToJSON Script where
    toJSON (RequireSignatureOf keyHash) = String $ bech32 keyHash
    toJSON (RequireAllOf content) =
        object ["all" .= fmap toJSON content]
    toJSON (RequireAnyOf content) =
        object ["any" .= fmap toJSON content]
    toJSON (RequireSomeOf count scripts) =
        object ["some" .= object ["at_least" .= count, "from" .= scripts]]

instance FromJSON Script where
    parseJSON v = parseKey v <|> parseAnyOf v  <|> parseAllOf v <|> parseAtLeast v
      where
        parseKey = withText "Script KeyHash" $
            either
                (fail . prettyErrKeyHashFromText)
                (pure . RequireSignatureOf)
                . keyHashFromText
        parseAnyOf = withObject "Script AnyOf" $ \o ->
            RequireAnyOf <$> o .: "any"
        parseAllOf = withObject "Script AllOf" $ \o ->
            RequireAllOf <$> o .: "all"
        parseAtLeast = withObject "Script SomeOf" $ \o -> do
            some <- o .: "some"
            RequireSomeOf <$> some .: "at_least" <*> some .: "from"
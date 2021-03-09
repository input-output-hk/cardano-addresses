{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Derivation
    (
    -- * Derivation Path
    -- ** Type
      DerivationPath
    , castDerivationPath
    , derivationPathToString
    , derivationPathFromString
    -- ** Applicative Parser
    , derivationPathArg

    -- * Derivation Index
    , DerivationIndex
    , mkDerivationIndex
    , firstHardened
    , indexToInteger
    , derivationIndexToString
    , derivationIndexFromString

    -- * XPub / XPrv
    , xpubReader
    , xpubOpt
    , xpubArg

    -- * Internal
    , bech32Reader
    ) where

import Prelude

import Cardano.Address.Derivation
    ( DerivationType (..)
    , Index
    , wholeDomainIndex
    , XPub
    , xpubFromBytes
    )
import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartToText )
import Codec.Binary.Encoding
    ( fromBech32 )
import Control.Arrow
    ( left )
import Control.Monad
    ( when )
import Data.ByteString
    ( ByteString )
import Data.List
    ( intercalate, isSuffixOf )
import Data.Word
    ( Word32 )
import Options.Applicative
    ( Parser
    , argument
    , completer
    , eitherReader
    , help
    , listCompleter
    , long
    , metavar
    , option
    )
import Safe
    ( readEitherSafe )
import System.IO.Extra
    ( markCharsRedAtIndices )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T


--
-- Derivation Path
--

-- | Represent a user-provided 'DerivationPath'.
newtype DerivationPath = DerivationPath [DerivationIndex]
    deriving (Show, Eq)

derivationPathFromString :: String -> Either String DerivationPath
derivationPathFromString str =
    DerivationPath
        <$> mapM (derivationIndexFromString . T.unpack) (T.splitOn "/" txt)
  where
    txt = T.pack str

derivationPathToString :: DerivationPath -> String
derivationPathToString (DerivationPath xs) =
    intercalate "/" $ map derivationIndexToString xs

castDerivationPath :: DerivationPath -> [Index 'WholeDomain depth]
castDerivationPath (DerivationPath xs) = map (wholeDomainIndex . getDerivationIndex) xs

derivationPathArg :: Parser DerivationPath
derivationPathArg = argument (eitherReader derivationPathFromString) $ mempty
    <> metavar "DERIVATION-PATH"
    <> help
        "Slash-separated derivation path. Hardened indexes are marked with a \
        \'H' (e.g. 1852H/1815H/0H/0)."
    <> completer (listCompleter ["1852H/1815H/0H/", "44H/1815H/0H/"])

--
-- Derivation Index
--

newtype DerivationIndex = DerivationIndex { getDerivationIndex :: Word32 }
    deriving stock   (Show, Eq)
    deriving newtype (Bounded, Enum, Ord)

-- | Safely cast a 'DerivationIndex' to an 'Integer'.
indexToInteger :: DerivationIndex -> Integer
indexToInteger (DerivationIndex ix) = fromIntegral ix

-- | Get the first 'DerivationIndex' considered /hardened/.
firstHardened :: DerivationIndex
firstHardened = DerivationIndex 0x80000000

-- | Smart-constructor for a 'DerivationIndex'
mkDerivationIndex :: Integer -> Either String DerivationIndex
mkDerivationIndex ix
    | ix > fromIntegral (maxBound @Word32) =
        Left $ show ix <> " is too high to be a derivation index."
    | otherwise =
        pure $ DerivationIndex $ fromIntegral ix

-- | Convert a string to a derivation index. String must be followed by a
-- capital /H/ to mark hardened index. For example @0@ refers to the first soft
-- index, whereas @0H@ refers to the first hardened index.
derivationIndexFromString :: String -> Either String DerivationIndex
derivationIndexFromString "" = Left "An empty string is not a derivation index!"
derivationIndexFromString str
    | "H" `isSuffixOf` str = do
        parseHardenedIndex (init str)
    | otherwise = do
        parseSoftIndex str
  where
    parseHardenedIndex txt = do
        ix <- left (const msg) $ readEitherSafe txt
        mkDerivationIndex $ ix + indexToInteger firstHardened
      where
        msg = mconcat
            [ "Unable to parse hardened index. Hardened indexes are integer "
            , "values, between "
            , show (indexToInteger (minBound @DerivationIndex))
            , " and "
            , show (indexToInteger firstHardened)
            , " ending with a capital 'H'. For example: \"42H\","
            ]

    parseSoftIndex txt = do
        ix <- left (const msg) $ readEitherSafe txt
        guardSoftIndex ix
        mkDerivationIndex ix
      where
        guardSoftIndex ix
            | ix >= indexToInteger firstHardened =
                Left $ mconcat
                    [ show ix
                    , " is too high to be a soft derivation index. "
                    , "Did you mean \""
                    , show (ix - indexToInteger firstHardened)
                    , "H\"?"
                    ]
            | otherwise =
                pure ()

        msg = mconcat
            [ "Unable to parse soft index. Soft indexes are integer "
            , "values, between "
            , show (indexToInteger (minBound @DerivationIndex))
            , " and "
            , show (indexToInteger firstHardened)
            , ". For example: \"14\"."
            ]

-- | Convert a 'DerivationIndex' back to string.
derivationIndexToString :: DerivationIndex -> String
derivationIndexToString ix_@(DerivationIndex ix)
    | ix_ >= firstHardened = show ix' ++ "H"
    | otherwise            = show ix
  where
    ix' = fromIntegral ix - indexToInteger firstHardened

--
-- XPub / XPrv
--

xpubReader :: [HumanReadablePart] -> String -> Either String XPub
xpubReader allowedPrefixes str = do
    (_hrp, bytes) <- bech32Reader allowedPrefixes str
    case xpubFromBytes bytes of
        Just xpub -> pure xpub
        Nothing   -> Left
            "Failed to convert bytes into a valid extended public key."

xpubOpt :: [HumanReadablePart] -> String -> String -> Parser XPub
xpubOpt allowedPrefixes name helpDoc =
    option (eitherReader (xpubReader allowedPrefixes)) $ mempty
        <> long name
        <> metavar "XPUB"
        <> help helpDoc

xpubArg :: [HumanReadablePart] -> String -> Parser XPub
xpubArg allowedPrefixes helpDoc =
    argument (eitherReader (xpubReader allowedPrefixes)) $ mempty
        <> metavar "XPUB"
        <> help helpDoc

--
-- Internal
--

bech32Reader
    :: [HumanReadablePart]
    -> String
    -> Either String (HumanReadablePart, ByteString)
bech32Reader allowedPrefixes str = do
    (hrp, bytes) <- fromBech32 markCharsRedAtIndices (toBytes str)
    when (hrp `notElem` allowedPrefixes) $ Left
        $ "Invalid human-readable prefix. Prefix ought to be one of: "
        <> show (showHrp <$> allowedPrefixes)
    pure (hrp, bytes)
  where
    showHrp :: HumanReadablePart -> String
    showHrp = T.unpack . humanReadablePartToText

    toBytes :: String -> ByteString
    toBytes = T.encodeUtf8 . T.pack

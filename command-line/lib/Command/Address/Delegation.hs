{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Delegation
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( Address (unAddress), unsafeMkAddress )
import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), ErrExtendAddress (..) )
import Options.Applicative
    ( CommandFields, Mod, command, footerDoc, helper, info, progDesc )
import Options.Applicative.Credential
    ( delegationCredentialArg )
import Options.Applicative.Help.Pretty
    ( bold, indent, string, vsep )
import System.IO
    ( stdin, stdout )
import System.IO.Extra
    ( hGetBech32, progName )

import qualified Cardano.Address.Style.Shelley as Shelley
import Cardano.Codec.Bech32
    ( bech32With )
import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

newtype Cmd = Cmd
    { credential :: Credential 'DelegationK
    } deriving Show

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "delegation" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a delegation address"
        <> footerDoc (Just $ vsep
            [ string "The payment address is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/2/0 > stake.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address payment --network-tag testnet \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address delegation $(cat stake.prv | "<>progName<>" key public --with-chain-code)"
            , indent 2 $ string "addr1qpj2d4dqzds5p3mmlu95v9pex2d72cdvyjh2u3dtj4yqesv27k..."
            ])
  where
    parser = Cmd
        <$> delegationCredentialArg "An extended stake public key or a script hash."

run :: Cmd -> IO ()
run Cmd{credential} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    case Shelley.extendAddress (unsafeMkAddress bytes) credential of
        Left (ErrInvalidAddressStyle msg) ->
            fail msg
        Left (ErrInvalidAddressType  msg) ->
            fail msg
        Right addr ->
           B8.hPutStr stdout $ T.encodeUtf8 $ bech32With hrp (unAddress addr)
  where
    allowedPrefixes =
        [ CIP5.addr
        , CIP5.addr_test
        ]

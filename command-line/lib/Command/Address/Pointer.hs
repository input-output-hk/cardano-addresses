{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Command.Address.Pointer
    ( Cmd
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Address
    ( Address (unAddress), ChainPointer (..), unsafeMkAddress )
import Cardano.Address.Style.Shelley
    ( Credential (..), ErrExtendAddress (..) )
import Numeric.Natural
    ( Natural )
import Options.Applicative
    ( CommandFields
    , Mod
    , argument
    , auto
    , command
    , footerDoc
    , header
    , help
    , helper
    , info
    , metavar
    , progDesc
    )
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

data Cmd = Cmd
    { slotNum :: Natural
    , transactionIndex :: Natural
    , outputIndex :: Natural
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "pointer" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Create a pointer address"
        <> header "Create addresses with a pointer that indicate the position \
            \of a registered stake address on the chain."
        <> footerDoc (Just $ vsep
            [ string "The payment address is read from stdin."
            , string ""
            , string "Example:"
            , indent 2 $ bold $ string $ "$ "<>progName<>" recovery-phrase generate --size 15 \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key from-recovery-phrase Shelley > root.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat root.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key child 1852H/1815H/0H/0/0 > addr.prv"
            , indent 2 $ string ""
            , indent 2 $ bold $ string "$ cat addr.prv \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" key public --with-chain-code \\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address payment --network-tag 0\\"
            , indent 4 $ bold $ string $ "| "<>progName<>" address pointer 42 14 0"
            , indent 2 $ string "addr1grq8e0smk44luyl897e24gn6qfkx4ax734r6pzq29zcew032pcqqef7zzu"
            ])
  where
    parser = Cmd
        <$> argument auto (metavar "SLOT" <> help "A slot number")
        <*> argument auto (metavar "TX"   <> help "A transaction index within that slot")
        <*> argument auto (metavar "OUT"  <> help "An output index within that transaction")

run :: Cmd -> IO ()
run Cmd{slotNum,transactionIndex,outputIndex} = do
    (hrp, bytes) <- hGetBech32 stdin allowedPrefixes
    case Shelley.extendAddress (unsafeMkAddress bytes) (DelegationFromPointer ptr) of
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

    ptr = ChainPointer { slotNum, transactionIndex, outputIndex  }

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Transaction where
import           Crypto.Hash.SHA256
import           Crypto.Secp256k1
import           Data.ByteString.Char8
import           Data.Maybe
import           Data.Monoid
import           Prelude

data UnsignedTrx = UnsignedTrx{
    unsignedTrxSource      :: Maybe PubKey,
    unsignedTrxDestination :: PubKey,
    unsignedTrxAmount      :: Integer,
    unsignedTrxUnixTime    :: Integer
  }

data Transaction = Transaction{
    transactionSource      :: Maybe PubKey,
    transactionDestination :: PubKey,
    transactionAmount      :: Integer,
    transactionUnixTime    :: Integer,
    transactionHash        :: Msg,
    transactionSignature   :: Sig
  }

mkTransaction :: UnsignedTrx -> SecKey -> Maybe Transaction
mkTransaction UnsignedTrx{
                unsignedTrxSource,
                unsignedTrxDestination,
                unsignedTrxAmount,
                unsignedTrxUnixTime
              }
              privKey =
  trxBuilder <$> maybeHash
  where
    trxBuilder :: Msg -> Transaction
    trxBuilder justHash = Transaction{
        transactionSource      = unsignedTrxSource,
        transactionDestination = unsignedTrxDestination,
        transactionAmount      = unsignedTrxAmount,
        transactionUnixTime    = unsignedTrxUnixTime,
        transactionHash        = justHash,
        transactionSignature   = signMsg privKey justHash
      }
    maybeHash :: Maybe Msg
    maybeHash = msg . hash . mconcat $ [
        maybe "" (exportPubKey True) unsignedTrxSource,
        exportPubKey True unsignedTrxDestination,
        pack $ show unsignedTrxAmount,
        pack $ show unsignedTrxUnixTime
      ]

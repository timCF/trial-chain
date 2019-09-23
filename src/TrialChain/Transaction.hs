{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Transaction where
import           Crypto.Hash.SHA256
import           Crypto.Secp256k1
import           Data.ByteString.Char8
import           Data.Maybe
import           Data.Monoid
import           Prelude

data UnhashedTrx = UnhashedTrx{
    unhashedTrxSource      :: Maybe PubKey,
    unhashedTrxDestination :: PubKey,
    unhashedTrxAmount      :: Integer,
    unhashedTrxUnixTime    :: Integer
  }

data Transaction = Transaction{
    transactionSource      :: Maybe PubKey,
    transactionDestination :: PubKey,
    transactionAmount      :: Integer,
    transactionUnixTime    :: Integer,
    transactionHash        :: Msg,
    transactionSignature   :: Sig
  }

mkTransaction :: UnhashedTrx -> SecKey -> Maybe Transaction
mkTransaction UnhashedTrx{
                unhashedTrxSource,
                unhashedTrxDestination,
                unhashedTrxAmount,
                unhashedTrxUnixTime
              }
              privKey =
  trxBuilder <$> maybeHash
  where
    trxBuilder :: Msg -> Transaction
    trxBuilder justHash = Transaction{
        transactionSource      = unhashedTrxSource,
        transactionDestination = unhashedTrxDestination,
        transactionAmount      = unhashedTrxAmount,
        transactionUnixTime    = unhashedTrxUnixTime,
        transactionHash        = justHash,
        transactionSignature   = signMsg privKey justHash
      }
    maybeHash :: Maybe Msg
    maybeHash = msg . hash . mconcat $ [
        maybe "" (exportPubKey True) unhashedTrxSource,
        exportPubKey True unhashedTrxDestination,
        pack $ show unhashedTrxAmount,
        pack $ show unhashedTrxUnixTime
      ]

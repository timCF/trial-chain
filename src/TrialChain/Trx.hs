{-# LANGUAGE DeriveGeneric #-}

module TrialChain.Trx
  ( mkP2PTrx
  , mkRewardTrx
  , getTrxHash
  , isValidTrx
  , CommonTrx(..)
  , Trx
  ) where

import Crypto.Hash.SHA256
import Crypto.Secp256k1
import Data.Binary (Binary)
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import Prelude

data CommonTrx =
  CommonTrx
    { commonTrxDestination :: ByteString
    , commonTrxAmount :: Integer
    , commonTrxUnixTime :: Integer
    }
  deriving (Generic)

instance Binary CommonTrx

data Trx
  = P2PTrx
      { p2pTrxCommon :: CommonTrx
      , p2pTrxSource :: ByteString
      , p2pTrxHash :: ByteString
      , p2pTrxSignature :: ByteString
      }
  | RewardTrx
      { rewardTrxCommon :: CommonTrx
      , rewardTrxHash :: ByteString
      }
  deriving (Generic)

instance Binary Trx

mkP2PTrx :: CommonTrx -> PubKey -> SecKey -> Maybe Trx
mkP2PTrx commonTrx sourcePubKey sourceSecKey =
  builder <$> msg (mkP2PTrxHash commonTrx sourcePubKey)
  where
    builder :: Msg -> Trx
    builder justHash =
      P2PTrx
        { p2pTrxCommon = commonTrx
        , p2pTrxSource = exportPubKey True sourcePubKey
        , p2pTrxHash = getMsg justHash
        , p2pTrxSignature = exportSig $ signMsg sourceSecKey justHash
        }

mkP2PTrxHash :: CommonTrx -> PubKey -> ByteString
mkP2PTrxHash commonTrx sourcePubKey =
  hash . mconcat $
  [ commonTrxDestination commonTrx
  , pack $ show (commonTrxAmount commonTrx)
  , pack $ show (commonTrxUnixTime commonTrx)
  , exportPubKey True sourcePubKey
  ]

mkRewardTrx :: CommonTrx -> Trx
mkRewardTrx commonTrx =
  RewardTrx {rewardTrxCommon = commonTrx, rewardTrxHash = mkRewardTrxHash commonTrx}

mkRewardTrxHash :: CommonTrx -> ByteString
mkRewardTrxHash commonTrx =
  hash . mconcat $
  [ commonTrxDestination commonTrx
  , pack $ show (commonTrxAmount commonTrx)
  , pack $ show (commonTrxUnixTime commonTrx)
  ]

isValidTrx :: Trx -> Bool
isValidTrx RewardTrx {rewardTrxCommon = commonTrx, rewardTrxHash = trxHash} =
  mkRewardTrxHash commonTrx == trxHash
isValidTrx P2PTrx { p2pTrxCommon = commonTrx
                  , p2pTrxHash = trxHash
                  , p2pTrxSource = trxSource
                  , p2pTrxSignature = trxSignature
                  } =
  case keySigMsg of
    Nothing -> False
    Just (pubKey, sig, thisMsg) ->
      trxHash == mkP2PTrxHash commonTrx pubKey && verifySig pubKey sig thisMsg
  where
    keySigMsg :: Maybe (PubKey, Sig, Msg)
    keySigMsg = do
      pubKey <- importPubKey trxSource
      sig <- importSig trxSignature
      thisMsg <- msg trxHash
      return (pubKey, sig, thisMsg)

getTrxHash :: Trx -> ByteString
getTrxHash P2PTrx {p2pTrxHash = x} = x
getTrxHash RewardTrx {rewardTrxHash = x} = x

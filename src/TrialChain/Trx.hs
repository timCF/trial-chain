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
import Data.Coerce
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import Prelude
import TrialChain.Misc

data CommonTrx =
  CommonTrx
    { commonTrxDestination :: Addr
    , commonTrxAmount :: Coins
    , commonTrxUnixTime :: UnixTime
    }
  deriving (Generic)

instance Binary CommonTrx

data Trx
  = P2PTrx
      { p2pTrxCommon :: CommonTrx
      , p2pTrxSource :: Addr
      , p2pTrxHash :: TrxHash
      , p2pTrxSignature :: Sign
      }
  | RewardTrx
      { rewardTrxCommon :: CommonTrx
      , rewardTrxHash :: TrxHash
      }
  deriving (Generic)

instance Binary Trx

mkP2PTrx :: CommonTrx -> PubKey -> SecKey -> Maybe Trx
mkP2PTrx commonTrx sourcePubKey sourceSecKey =
  builder <$> msg (coerce $ mkP2PTrxHash commonTrx sourcePubKey)
  where
    builder :: Msg -> Trx
    builder justHash =
      P2PTrx
        { p2pTrxCommon = commonTrx
        , p2pTrxSource = Addr $ exportPubKey True sourcePubKey
        , p2pTrxHash = TrxHash $ getMsg justHash
        , p2pTrxSignature = Sign . exportSig $ signMsg sourceSecKey justHash
        }

mkP2PTrxHash :: CommonTrx -> PubKey -> TrxHash
mkP2PTrxHash commonTrx sourcePubKey =
  TrxHash . hash . mconcat $
  [ coerce commonTrxDestination commonTrx
  , pack $ show (coerce $ commonTrxAmount commonTrx :: Integer)
  , pack $ show (coerce $ commonTrxUnixTime commonTrx :: Integer)
  , exportPubKey True sourcePubKey
  ]

mkRewardTrx :: CommonTrx -> Trx
mkRewardTrx commonTrx =
  RewardTrx {rewardTrxCommon = commonTrx, rewardTrxHash = mkRewardTrxHash commonTrx}

mkRewardTrxHash :: CommonTrx -> TrxHash
mkRewardTrxHash commonTrx =
  TrxHash . hash . mconcat $
  [ coerce commonTrxDestination commonTrx
  , pack $ show (coerce $ commonTrxAmount commonTrx :: Integer)
  , pack $ show (coerce $ commonTrxUnixTime commonTrx :: Integer)
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
      pubKey <- importPubKey $ coerce trxSource
      sig <- importSig $ coerce trxSignature
      thisMsg <- msg $ coerce trxHash
      return (pubKey, sig, thisMsg)

getTrxHash :: Trx -> TrxHash
getTrxHash P2PTrx {p2pTrxHash = x} = x
getTrxHash RewardTrx {rewardTrxHash = x} = x

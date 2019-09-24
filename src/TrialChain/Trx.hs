module TrialChain.Trx
  ( mkP2PTrx
  , mkRewardTrx
  , getTrxHash
  , CommonTrx(..)
  , Trx
  ) where

import Crypto.Hash.SHA256
import Crypto.Secp256k1
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import Prelude

data CommonTrx = CommonTrx
  { commonTrxDestination :: PubKey
  , commonTrxAmount :: Integer
  , commonTrxUnixTime :: Integer
  }

data Trx
  = P2PTrx { p2pTrxCommon :: CommonTrx
           , p2pTrxSource :: PubKey
           , p2pTrxHash :: Msg
           , p2pTrxSignature :: Sig }
  | RewardTrx { rewardTrxCommon :: CommonTrx
              , rewardTrxHash :: ByteString }

mkP2PTrx :: CommonTrx -> PubKey -> SecKey -> Maybe Trx
mkP2PTrx commonTrx sourcePubKey sourceSecKey = builder <$> mhash
  where
    builder :: Msg -> Trx
    builder justHash =
      P2PTrx
        { p2pTrxCommon = commonTrx
        , p2pTrxSource = sourcePubKey
        , p2pTrxHash = justHash
        , p2pTrxSignature = signMsg sourceSecKey justHash
        }
    mhash :: Maybe Msg
    mhash =
      msg . hash . mconcat $
      [ exportPubKey True (commonTrxDestination commonTrx)
      , pack $ show (commonTrxAmount commonTrx)
      , pack $ show (commonTrxUnixTime commonTrx)
      , exportPubKey True sourcePubKey
      ]

mkRewardTrx :: CommonTrx -> Trx
mkRewardTrx commonTrx =
  RewardTrx {rewardTrxCommon = commonTrx, rewardTrxHash = justHash}
  where
    justHash :: ByteString
    justHash =
      hash . mconcat $
      [ exportPubKey True (commonTrxDestination commonTrx)
      , pack $ show (commonTrxAmount commonTrx)
      , pack $ show (commonTrxUnixTime commonTrx)
      ]

getTrxHash :: Trx -> ByteString
getTrxHash P2PTrx {p2pTrxHash = x} = getMsg x
getTrxHash RewardTrx {rewardTrxHash = x} = x

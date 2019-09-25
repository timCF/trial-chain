module TrialChain.Chain where

import Crypto.Secp256k1
import Data.ByteString
import Prelude
import TrialChain.Block
import TrialChain.Trx

data Chain = Chain
  { chainBlocks :: [Block]
  , chainDifficulty :: Integer
  , chainPendingTrxs :: [Trx]
  , chainRewardAmount :: Integer
  , chainRewardDestination :: PubKey
  , chainNonce :: Integer
  }

mineChain :: Integer -> Chain -> Either Chain Block
mineChain unixTime chain =
  case mineBlock (chainDifficulty chain) commonBlock of
    Left failedBlock -> Left chain {chainNonce = blockNonce failedBlock}
    Right minedBlock -> Right minedBlock
  where
    prevBlockHash :: ByteString
    prevBlockHash =
      case chainBlocks chain of
        [] -> mempty
        (prevBlock:_) -> blockHash prevBlock
    rewardTrx :: Trx
    rewardTrx =
      mkRewardTrx
        CommonTrx
          { commonTrxDestination = chainRewardDestination chain
          , commonTrxAmount = chainRewardAmount chain
          , commonTrxUnixTime = unixTime
          }
    commonBlock :: CommonBlock
    commonBlock =
      CommonBlock
        { blockPrevHash = prevBlockHash
        , blockUnixTime = unixTime
        , blockTrxs = rewardTrx : chainPendingTrxs chain
        , blockNonce = chainNonce chain
        }

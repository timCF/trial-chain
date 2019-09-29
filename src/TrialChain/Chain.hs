module TrialChain.Chain where

import Crypto.Secp256k1
import Data.ByteString hiding (length)
import Prelude
import TrialChain.Block
import TrialChain.Trx

data Chain =
  Chain
    { chainBlocks :: [Block]
    , chainDifficulty :: Integer
    , chainPendingTrxs :: [Trx]
    , chainRewardAmount :: Integer
    , chainRewardDestination :: PubKey
    , chainNonce :: Integer
    }

mergeChain :: [Block] -> Chain -> Chain
mergeChain incomingBlocks chain =
  if length incomingBlocks > length (chainBlocks chain) && isValidChain newChain
    then newChain
    else chain
  where
    newChain :: Chain
    newChain = chain {chainBlocks = incomingBlocks}

--
--  TODO : validate blocks
--  TODO : implement
--
isValidChain :: Chain -> Bool
isValidChain _ = True

mkChain :: PubKey -> Chain
mkChain rewardDestination =
  Chain
    { chainBlocks = []
    , chainDifficulty = 3
    , chainPendingTrxs = []
    , chainRewardAmount = 20
    , chainRewardDestination = rewardDestination
    , chainNonce = 0
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
          { commonTrxDestination = exportPubKey True $ chainRewardDestination chain
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

module TrialChain.Chain where
import           Crypto.Secp256k1
import           Prelude
import           TrialChain.Block
import           TrialChain.Trx

data Chain = Chain{
    chainBlocks            :: [Block],
    chainDifficulty        :: Integer,
    chainPendingTrxs       :: [Trx],
    chainRewardAmount      :: Integer,
    chainRewardDestination :: PubKey
  }

mine :: Chain -> Integer -> Chain
mine chain unixTime =
  go chain{chainPendingTrxs = rewardTrx : chainPendingTrxs chain}
  where
    rewardTrx :: Trx
    rewardTrx = mkRewardTrx CommonTrx{
      commonTrxDestination = chainRewardDestination chain,
      commonTrxAmount      = chainRewardAmount chain,
      commonTrxUnixTime    = unixTime
    }
    go :: Chain -> Chain
    --
    --  TODO : !!!!
    --
    go = id

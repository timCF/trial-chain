module TrialChain.Chain where
import           Crypto.Secp256k1
import           Prelude
import           TrialChain.Block
import           TrialChain.Transaction

data Chain = Chain{
    chainBlocks              :: [Block],
    chainDifficulty          :: Integer,
    chainPendingTransactions :: [Transaction],
    chainRewardAmount        :: Integer,
    chainRewardDestination   :: PubKey
  }

-- mine :: Chain -> Chain
-- mine chain =
--   undefined
--   where
--     rewardTransaction :: Transaction
--     rewardTransaction =

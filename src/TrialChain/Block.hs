module TrialChain.Block where
import           Data.ByteString
import           Prelude
import           TrialChain.Transaction

data Block = Block{
    blockPrevHash     :: ByteString,
    blockUnixTime     :: Integer,
    blockTransactions :: [Transaction],
    blockNonce        :: Integer,
    blockHash         :: ByteString
  }

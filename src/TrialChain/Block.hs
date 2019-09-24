module TrialChain.Block where
import           Data.ByteString
import           Prelude
import           TrialChain.Trx

data Block = Block{
    blockPrevHash :: ByteString,
    blockUnixTime :: Integer,
    blockTrxs     :: [Trx],
    blockNonce    :: Integer,
    blockHash     :: ByteString
  }

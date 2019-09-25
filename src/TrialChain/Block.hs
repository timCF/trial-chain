module TrialChain.Block where

import Crypto.Hash.SHA256
import Data.ByteString (replicate)
import Data.ByteString.Char8 hiding (replicate)
import Data.Monoid
import Prelude hiding (replicate)
import TrialChain.Trx

data CommonBlock = CommonBlock
  { blockPrevHash :: ByteString
  , blockUnixTime :: Integer
  , blockTrxs :: [Trx]
  , blockNonce :: Integer
  }

data Block = Block
  { blockCommon :: CommonBlock
  , blockHash :: ByteString
  }

mineBlock :: Integer -> CommonBlock -> Either CommonBlock Block
mineBlock difficulty commonBlock = work oldNonce
  where
    work :: Integer -> Either CommonBlock Block
    work newNonce
      | newNonce > nonceLimit = Left commonBlock {blockNonce = newNonce}
      | otherwise =
        if desiredPrefix `isPrefixOf` newHash
          then Right
                 Block
                   { blockCommon = commonBlock {blockNonce = newNonce}
                   , blockHash = newHash
                   }
          else work $ newNonce + 1
      where
        newHash :: ByteString
        newHash = hash $ read (show newNonce) <> bytes2hash
    oldNonce :: Integer
    oldNonce = blockNonce commonBlock
    nonceLimit :: Integer
    nonceLimit = oldNonce + 10000
    desiredPrefix :: ByteString
    desiredPrefix = replicate (fromInteger difficulty) 48
    bytes2hash :: ByteString
    bytes2hash =
      mconcat
        [ blockPrevHash commonBlock
        , read $ show (blockUnixTime commonBlock)
        , mconcat $ getTrxHash <$> blockTrxs commonBlock
        ]

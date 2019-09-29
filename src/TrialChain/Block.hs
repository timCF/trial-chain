{-# LANGUAGE DeriveGeneric #-}

module TrialChain.Block where

import Crypto.Hash.SHA256
import Data.Binary (Binary)
import Data.ByteString (replicate)
import Data.ByteString.Char8 hiding (replicate)
import Data.Monoid
import GHC.Generics (Generic)
import Prelude hiding (replicate)
import TrialChain.Trx

data CommonBlock =
  CommonBlock
    { blockPrevHash :: ByteString
    , blockUnixTime :: Integer
    , blockTrxs :: [Trx]
    , blockNonce :: Integer
    }
  deriving (Generic)

instance Binary CommonBlock

data Block =
  Block
    { blockCommon :: CommonBlock
    , blockHash :: ByteString
    }
  deriving (Generic)

instance Binary Block

mineBlock :: Integer -> CommonBlock -> Either CommonBlock Block
mineBlock difficulty commonBlock =
  let nonce = blockNonce commonBlock
   in work (nonce + 999999) nonce
  where
    work :: Integer -> Integer -> Either CommonBlock Block
    work limit thisNonce
      | thisNonce > limit = Left newCommonBlock
      | otherwise =
        if isValidBlockHash difficulty thisHash
          then Right Block {blockCommon = newCommonBlock, blockHash = thisHash}
          else work limit (thisNonce + 1)
      where
        newCommonBlock :: CommonBlock
        newCommonBlock = commonBlock {blockNonce = thisNonce}
        thisHash :: ByteString
        thisHash = mkBlockHash newCommonBlock

isValidBlockHash :: Integer -> ByteString -> Bool
isValidBlockHash difficulty thisHash = desiredPrefix `isPrefixOf` thisHash
  where
    desiredPrefix :: ByteString
    desiredPrefix = replicate (fromInteger difficulty) 0

isValidBlock :: Integer -> Block -> Bool
isValidBlock difficulty Block {blockCommon = commonBlock, blockHash = thisHash} =
  isValidBlockHash difficulty thisHash && mkBlockHash commonBlock == thisHash

mkBlockHash :: CommonBlock -> ByteString
mkBlockHash commonBlock =
  hash $
  mconcat
    [ blockPrevHash commonBlock
    , pack $ show (blockUnixTime commonBlock)
    , mconcat $ getTrxHash <$> blockTrxs commonBlock
    , pack $ show (blockNonce commonBlock)
    ]

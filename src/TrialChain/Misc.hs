{-# LANGUAGE DeriveGeneric #-}

module TrialChain.Misc where

import Data.Binary (Binary)
import Data.ByteString
import GHC.Generics (Generic)
import Prelude

newtype Addr =
  Addr ByteString
  deriving (Generic)

instance Binary Addr

newtype BlockHash =
  BlockHash ByteString
  deriving (Generic)

instance Binary BlockHash

newtype TrxHash =
  TrxHash ByteString
  deriving (Generic)

instance Binary TrxHash

newtype Sign =
  Sign ByteString
  deriving (Generic)

instance Binary Sign

newtype Coins =
  Coins Integer
  deriving (Generic)

instance Binary Coins

newtype UnixTime =
  UnixTime Integer
  deriving (Generic)

instance Binary UnixTime

newtype Difficulty =
  Difficulty Integer
  deriving (Generic)

instance Binary Difficulty

{-# LANGUAGE DeriveGeneric #-}

module TrialChain.Misc
  ( Addr(..)
  , BlockHash(..)
  , TrxHash(..)
  , Sign(..)
  , Coins(..)
  , UnixTime(..)
  , Difficulty(..)
  , Balance
  , mkBalance
  , addCoins
  , transferCoins
  ) where

import Data.Binary (Binary)
import Data.ByteString hiding (empty)
import Data.Coerce
import Data.Map
import Data.Maybe
import GHC.Generics (Generic)
import Prelude hiding (lookup)

newtype Addr =
  Addr ByteString
  deriving (Generic, Eq, Ord)

instance Binary Addr

newtype BlockHash =
  BlockHash ByteString
  deriving (Generic, Eq)

instance Binary BlockHash

newtype TrxHash =
  TrxHash ByteString
  deriving (Generic, Eq)

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

newtype Balance =
  Balance (Map Addr Coins)

mkBalance :: Balance
mkBalance = Balance empty

addCoins :: Addr -> Coins -> Balance -> Balance
addCoins addr coins bs =
  Balance $ insertWith (\x y -> Coins $ coerce x + coerce y) addr coins (coerce bs)

transferCoins :: Addr -> Addr -> Coins -> Balance -> Maybe Balance
transferCoins sourceAddr destinationAddr coins bs =
  case oldBalance - coerce coins of
    balance
      | balance >= 0 ->
        Just . addCoins destinationAddr coins . Balance $
        insert sourceAddr (Coins balance) (coerce bs)
    _ -> Nothing
  where
    oldBalance :: Integer
    oldBalance = maybe 0 (coerce :: Coins -> Integer) (lookup sourceAddr (coerce bs))

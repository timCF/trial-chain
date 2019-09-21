{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Node (start, newTrx) where
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process

start :: Process ()
start = getSelfPid >>= register alias >> loop []

alias :: String
alias = "TrialChain.Node"

newTrx :: Integer -> Process ()
newTrx = P2P.nsendPeers alias

loop :: [Integer] -> Process ()
loop state =
  receiveWait
    [
      match (\x -> do
        let newState = x:state
        say $ show newState
        loop newState
      )
    ]

{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Node (start, mkNodeApi, NodeApi(..)) where
import           Control.Concurrent.MVar                    (newMVar, swapMVar,
                                                             takeMVar)
import qualified Control.Distributed.Backend.P2P            as P2P
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types (LocalNode)
import           Control.Distributed.Process.Node           (runProcess)

start :: Process ()
start = getSelfPid >>= register alias >> loop []

alias :: String
alias = "TrialChain.Node"

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

newTrx :: (Integer -> IO (Maybe Integer)) -> Integer -> Process ()
newTrx setter it = do
  P2P.nsendPeers alias it
  _ <- liftIO $ setter it
  return ()

newtype NodeApi = NodeApi{broadcastTrx :: Integer -> IO Integer}

mkNodeApi :: LocalNode -> IO NodeApi
mkNodeApi node = do
  mvar <- newMVar Nothing
  let setter it = swapMVar mvar (Just it)
  let getter = takeMVar mvar
  let broadcastTrxApi it = do
        runProcess node (newTrx setter it)
        Just x <- getter
        return x
  return NodeApi{broadcastTrx = broadcastTrxApi}

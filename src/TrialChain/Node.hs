{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module TrialChain.Node (start, mkNodeApi, NodeApi(..)) where
import           Control.Concurrent.MVar                    (newMVar, swapMVar,
                                                             takeMVar)
import qualified Control.Distributed.Backend.P2P            as P2P
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types (LocalNode)
import           Control.Distributed.Process.Node           (runProcess)
import           Data.Binary                                (Binary)
import           GHC.Generics                               (Generic)
import           Prelude

data Msg = NewTrx Integer | EchoMsg String deriving (Generic)
instance Binary Msg

start :: Process ()
start = getSelfPid >>= register alias >> loop []

alias :: String
alias = "TrialChain.Node"

loop :: [Integer] -> Process ()
loop state = receiveWait [match handleMsg ]
  where
    handleMsg :: Msg -> Process ()
    handleMsg (NewTrx x) = do
      let newState = x:state
      say $ show newState
      loop newState
    handleMsg (EchoMsg x) = do
      say x
      loop state

--
--  TODO : fix code duplication
--

newTrx :: (Integer -> IO (Maybe Integer)) -> Integer -> Process ()
newTrx setter it = do
  P2P.nsendPeers alias (NewTrx it)
  _ <- liftIO $ setter it
  return ()

echoMsgFun :: (String -> IO (Maybe String)) -> String -> Process ()
echoMsgFun setter it = do
  P2P.nsendPeers alias (EchoMsg it)
  _ <- liftIO $ setter it
  return ()

data NodeApi = NodeApi{
  broadcastTrx :: Integer -> IO Integer,
  echoMsg      :: String -> IO String
}

mkNodeApi :: LocalNode -> IO NodeApi
mkNodeApi node = do
  let broadcastTrxApi it = do
        mvar <- newMVar Nothing
        let setter this = swapMVar mvar (Just this)
        let getter = takeMVar mvar
        runProcess node (newTrx setter it)
        --
        --  TODO : fix unsafe code here
        --
        Just x <- getter
        return x
  let echoMsgApi it = do
        mvar <- newMVar Nothing
        let setter this = swapMVar mvar (Just this)
        let getter = takeMVar mvar
        runProcess node (echoMsgFun setter it)
        --
        --  TODO : fix unsafe code here
        --
        Just x <- getter
        return x
  return NodeApi{
    broadcastTrx = broadcastTrxApi,
    echoMsg = echoMsgApi
  }

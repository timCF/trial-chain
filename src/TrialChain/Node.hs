{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Node
  ( start
  , mkNodeApi
  , NodeApi(..)
  ) where

import Control.Concurrent.MVar (newMVar, swapMVar, takeMVar)
import qualified Control.Distributed.Backend.P2P as P2P
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (LocalNode)
import Control.Distributed.Process.Node (runProcess)
import Control.Monad (void)
import Crypto.Secp256k1 hiding (Msg)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Prelude

data MsgData
  = NewTrx Integer
  | EchoMsg String
  | Mine
  deriving (Generic)

instance Binary MsgData

data Msg = Msg
  { msgSource :: ProcessId
  , msgData :: MsgData
  } deriving (Generic)

instance Binary Msg

start :: PubKey -> Process ()
start _ = do
  self <- getSelfPid
  send self Msg {msgSource = self, msgData = Mine}
  register pidAlias self
  loop []

pidAlias :: String
pidAlias = "TrialChain.Node"

loop :: [Integer] -> Process ()
loop state = receiveWait [match handleMsg]
  where
    handleMsg :: Msg -> Process ()
    handleMsg Msg {msgData = (NewTrx x)} = do
      let newState = x : state
      say $ show newState
      loop newState
    handleMsg Msg {msgData = (EchoMsg x)} = do
      say x
      loop state
    --
    --  TODO : Fix!!! Other nodes can send this message and DDOS this node mailbox
    --
    handleMsg Msg {msgData = Mine} = do
      self <- getSelfPid
      send self Msg {msgData = Mine, msgSource = self}
      -- say "ARBEITEN!!!"
      loop state

data NodeApi = NodeApi
  { broadcastTrx :: Integer -> IO (Maybe Integer)
  , echoMsg :: String -> IO (Maybe String)
  }

mkNodeApi :: LocalNode -> ProcessId -> NodeApi
mkNodeApi node processId =
  NodeApi {broadcastTrx = mkMethod NewTrx, echoMsg = mkMethod EchoMsg}
  where
    mkMethod :: (a -> MsgData) -> (a -> IO (Maybe a))
    mkMethod mapper it = do
      (setter, getter) <- mkSetterGetter
      runProcess
        node
        (processFun it setter (\x -> Msg {msgData = mapper x, msgSource = processId}))
      getter

type Setter a = a -> IO ()

type Getter a = IO (Maybe a)

mkSetterGetter :: IO (Setter a, Getter a)
mkSetterGetter = do
  mvar <- newMVar Nothing
  let setter this = void $ swapMVar mvar (Just this)
  let getter = takeMVar mvar
  return (setter, getter)

processFun :: a -> Setter a -> (a -> Msg) -> Process ()
processFun it setter mapper = do
  P2P.nsendPeers pidAlias (mapper it)
  _ <- liftIO $ setter it
  return ()

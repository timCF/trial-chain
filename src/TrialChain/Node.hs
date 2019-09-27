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
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8 hiding (putStrLn)
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.UUID
import Data.UUID.V4
import GHC.Generics (Generic)
import Prelude
import TrialChain.Block
import TrialChain.Chain

data MsgData
  = NewTrx Integer
  | EchoMsg String
  | Mine
  deriving (Generic)

instance Binary MsgData

data Msg = Msg
  { msgSender :: ProcessId
  , msgData :: MsgData
  , msgSenderUuid :: Maybe UUID
  } deriving (Generic)

instance Binary Msg

data State = State
  { stateUuid :: UUID
  , stateChain :: Chain
  , stateSelfPid :: ProcessId
  }

start :: PubKey -> Process ()
start rewardDestination = do
  self <- getSelfPid
  uuid <- liftIO nextRandom
  send self Msg {msgSender = self, msgData = Mine, msgSenderUuid = Just uuid}
  register pidAlias self
  loop
    State
      {stateUuid = uuid, stateSelfPid = self, stateChain = mkChain rewardDestination}

pidAlias :: String
pidAlias = "TrialChain.Node"

loop :: State -> Process ()
loop state = receiveWait [match handleMsg]
  where
    handleMsg :: Msg -> Process ()
    handleMsg Msg {msgData = (NewTrx x), msgSender = senderPid} =
      serveOther senderPid Nothing state (handleMsgNewTrx x)
    handleMsg Msg {msgData = (EchoMsg x), msgSender = senderPid} =
      serveOther senderPid Nothing state (handleMsgEchoMsg x)
    handleMsg Msg {msgData = Mine, msgSender = senderPid, msgSenderUuid = senderUuid} =
      serveSelf senderPid senderUuid state handleMsgMine

handleMsgNewTrx :: Integer -> State -> Process ()
handleMsgNewTrx x state = do
  say $ show x
  loop state

handleMsgEchoMsg :: String -> State -> Process ()
handleMsgEchoMsg x state = do
  say x
  loop state

handleMsgMine :: State -> Process ()
handleMsgMine state = do
  let chain = stateChain state
  let self = stateSelfPid state
  send
    self
    Msg {msgData = Mine, msgSender = self, msgSenderUuid = Just $ stateUuid state}
  unixTime <- liftIO $ round <$> getPOSIXTime
  case mineChain unixTime chain of
    Left newChain -> do
      liftIO . putStrLn $ "new nonce = " <> show (chainNonce newChain)
      loop state {stateChain = newChain}
    Right newBlock -> do
      liftIO . putStrLn $ "new block = " <> unpack (Hex.encode $ blockHash newBlock)
      let newChain = chain {chainBlocks = newBlock : chainBlocks chain}
      loop state {stateChain = newChain}

serveSelf :: ProcessId -> Maybe UUID -> State -> (State -> Process ()) -> Process ()
serveSelf senderPid senderUuid state work = do
  let senderUuidMatch = (== stateUuid state) <$> senderUuid
  if senderPid == stateSelfPid state && (Just True == senderUuidMatch)
    then work state
    else loop state

serveOther :: ProcessId -> Maybe UUID -> State -> (State -> Process ()) -> Process ()
serveOther senderPid _ state work =
  if senderPid /= stateSelfPid state
    then work state
    else loop state

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
        (processFun
           it
           setter
           (\x ->
              Msg {msgData = mapper x, msgSender = processId, msgSenderUuid = Nothing}))
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

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
import Data.Coerce
import Data.Maybe
import Data.Monoid
import Data.Time.Clock.POSIX
import Data.UUID
import Data.UUID.V4
import GHC.Generics (Generic)
import Prelude
import TrialChain.Block
import TrialChain.Chain
import TrialChain.Misc

data MsgData
  = NewTrx Integer
  | EchoMsg String
  | MsgMine
  | MsgIncomingBlocks [Block]
  deriving (Generic)

instance Binary MsgData

data MsgCommon =
  MsgCommon
    { msgSenderPid :: ProcessId
    , msgSenderUuid :: Maybe UUID
    }
  deriving (Generic)

instance Binary MsgCommon

data Msg =
  Msg
    { msgCommon :: MsgCommon
    , msgData :: MsgData
    }
  deriving (Generic)

instance Binary Msg

data State =
  State
    { stateSelfPid :: ProcessId
    , stateSelfUuid :: UUID
    , stateChain :: Chain
    }

start :: PubKey -> Process ()
start rewardDestination = do
  self <- getSelfPid
  uuid <- liftIO nextRandom
  let chain = mkChain rewardDestination
  let state = State {stateSelfUuid = uuid, stateSelfPid = self, stateChain = chain}
  sendMineMsg state
  register pidAlias self
  loop state

sendMineMsg :: State -> Process ()
sendMineMsg state = do
  let self = stateSelfPid state
  let uuid = stateSelfUuid state
  let commonMsg = MsgCommon {msgSenderPid = self, msgSenderUuid = Just uuid}
  send self Msg {msgCommon = commonMsg, msgData = MsgMine}

pidAlias :: String
pidAlias = "TrialChain.Node"

loop :: State -> Process ()
loop state = receiveWait [match handleMsg]
  where
    handleMsg :: Msg -> Process ()
    handleMsg Msg {msgData = MsgMine, msgCommon = commonMsg} =
      serveSelf commonMsg state handleMsgMine
    handleMsg Msg {msgData = (NewTrx x), msgCommon = commonMsg} =
      serveOther commonMsg state (handleMsgNewTrx x)
    handleMsg Msg {msgData = (EchoMsg x), msgCommon = commonMsg} =
      serveOther commonMsg state (handleMsgEchoMsg x)
    handleMsg Msg {msgData = MsgIncomingBlocks bs, msgCommon = commonMsg} =
      serveOther commonMsg state (handleMsgIncomingBlocks bs)

handleMsgIncomingBlocks :: [Block] -> State -> Process ()
handleMsgIncomingBlocks blocks state =
  loop state {stateChain = mergeChain blocks (stateChain state)}

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
  sendMineMsg state
  let chain = stateChain state
  unixTime <- liftIO $ UnixTime . round <$> getPOSIXTime
  case mineChain unixTime chain of
    Left newChain -> do
      liftIO . putStrLn $ "new nonce = " <> show (chainNonce newChain)
      loop state {stateChain = newChain}
    Right newBlock -> do
      liftIO . putStrLn $
        "new block = " <> unpack (Hex.encode . coerce $ blockHash newBlock)
      let newChain = chain {chainBlocks = newBlock : chainBlocks chain}
      loop state {stateChain = newChain}

serveSelf :: MsgCommon -> State -> (State -> Process ()) -> Process ()
serveSelf commonMsg state work = do
  let senderUuidMatch = (== stateSelfUuid state) <$> msgSenderUuid commonMsg
  if stateSelfPid state == msgSenderPid commonMsg && fromMaybe False senderUuidMatch
    then work state
    else loop state

serveOther :: MsgCommon -> State -> (State -> Process ()) -> Process ()
serveOther commonMsg state work =
  if stateSelfPid state /= msgSenderPid commonMsg
    then work state
    else loop state

data NodeApi =
  NodeApi
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
              Msg
                { msgData = mapper x
                , msgCommon =
                    MsgCommon {msgSenderPid = processId, msgSenderUuid = Nothing}
                }))
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

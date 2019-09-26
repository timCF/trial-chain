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
  { msgSource :: ProcessId
  , msgData :: MsgData
  } deriving (Generic)

instance Binary Msg

start :: PubKey -> Process ()
start rewardDestination = do
  self <- getSelfPid
  send self Msg {msgSource = self, msgData = Mine}
  register pidAlias self
  loop $ mkChain rewardDestination

pidAlias :: String
pidAlias = "TrialChain.Node"

loop :: Chain -> Process ()
loop chain = receiveWait [match handleMsg]
  where
    handleMsg :: Msg -> Process ()
    handleMsg Msg {msgData = (NewTrx x), msgSource = sourcePid} =
      serveOther
        sourcePid
        chain
        (\_ -> do
           say $ show x
           loop chain)
    handleMsg Msg {msgData = (EchoMsg x), msgSource = sourcePid} =
      serveOther
        sourcePid
        chain
        (\_ -> do
           say x
           loop chain)
    handleMsg Msg {msgData = Mine, msgSource = sourcePid} =
      serveSelf
        sourcePid
        chain
        (\self -> do
           send self Msg {msgData = Mine, msgSource = self}
           unixTime <- liftIO $ round <$> getPOSIXTime
           case mineChain unixTime chain of
             Left newChain -> do
               liftIO . putStrLn $ "new nonce = " <> show (chainNonce newChain)
               loop newChain
             Right newBlock -> do
               liftIO . putStrLn $
                 "new block = " <> unpack (Hex.encode $ blockHash newBlock)
               loop chain {chainBlocks = newBlock : chainBlocks chain})

serveSelf :: ProcessId -> Chain -> (ProcessId -> Process ()) -> Process ()
serveSelf sourcePid chain work = do
  self <- getSelfPid
  if sourcePid == self
    then work self
    else loop chain

serveOther :: ProcessId -> Chain -> (ProcessId -> Process ()) -> Process ()
serveOther sourcePid chain work = do
  self <- getSelfPid
  if sourcePid /= self
    then work self
    else loop chain

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

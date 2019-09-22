{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TrialChain.Node (start, mkNodeApi, NodeApi(..)) where
import           Control.Concurrent.MVar                    (newMVar, swapMVar,
                                                             takeMVar)
import qualified Control.Distributed.Backend.P2P            as P2P
import           Control.Distributed.Process
import           Control.Distributed.Process.Internal.Types (LocalNode)
import           Control.Distributed.Process.Node           (runProcess)
import           Control.Monad                              (void)
import           Data.Binary                                (Binary)
import           GHC.Generics                               (Generic)
import           Prelude

data Msg = NewTrx Integer | EchoMsg String deriving (Generic)
instance Binary Msg

start :: Process ()
start = getSelfPid >>= register pidAlias >> loop []

pidAlias :: String
pidAlias = "TrialChain.Node"

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

data NodeApi = NodeApi{
  broadcastTrx :: Integer -> IO (Maybe Integer),
  echoMsg      :: String -> IO (Maybe String)
}

mkNodeApi :: LocalNode -> NodeApi
mkNodeApi node = NodeApi{
    broadcastTrx = mkMethod NewTrx,
    echoMsg = mkMethod EchoMsg
  }
  where
    mkMethod :: (a -> Msg) -> (a -> IO (Maybe a))
    mkMethod mapper it = do
      (setter, getter) <- mkSetterGetter
      runProcess node (processFun it setter mapper)
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

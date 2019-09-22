{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Trans    (liftIO)
import           Data.ByteString.Lazy
import           Network.JsonRpc.Server
import           Prelude
import           TrialChain.Node        (NodeApi (..))

apply :: ByteString -> NodeApi -> IO (Maybe ByteString)
apply req nodeApi = call (methods nodeApi) req

methods :: NodeApi -> [Method IO]
methods nodeApi =
  (\x -> x nodeApi) <$>
  [
    add . broadcastTrx,
    echo . echoMsg
  ]

add :: (Integer -> IO Integer) -> Method IO
add apiMethod = toMethod "add" f (Required "x" :+: Required "y" :+: ())
  where
    f :: Integer -> Integer -> RpcResult IO Integer
    f x y = liftIO $ apiMethod $ x + y

echo :: (String -> IO String) -> Method IO
echo apiMethod = toMethod "echo" f (Required "x" :+: ())
  where
    f :: String -> RpcResult IO String
    f x = liftIO $ apiMethod x

{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Except   (throwError)
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
    broadcastTrxMethod,
    echo
  ]

broadcastTrxMethod :: NodeApi -> Method IO
broadcastTrxMethod nodeApi =
  toMethod
    "blockchain.transaction.broadcast"
    f
    (Required "inputs" :+: Required "outputs" :+: ())
  where
    f :: Integer -> Integer -> RpcResult IO Integer
    f x y = liftIO (broadcastTrx nodeApi $ x + y) >>= unwrapRes

echo :: NodeApi -> Method IO
echo nodeApi = toMethod "echo" f (Required "x" :+: ())
  where
    f :: String -> RpcResult IO String
    f x = liftIO (echoMsg nodeApi x) >>= unwrapRes

unwrapRes :: Maybe a -> RpcResult IO a
unwrapRes (Just it) = return it
unwrapRes Nothing   = throwError $ rpcError (-32603) "internal error"

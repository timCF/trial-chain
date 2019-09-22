{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Except   (ExceptT, throwError)
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
    add,
    echo
  ]

add :: NodeApi -> Method IO
add nodeApi = toMethod "add" f (Required "x" :+: Required "y" :+: ())
  where
    f :: Integer -> Integer -> RpcResult IO Integer
    f x y = do
      res <- liftIO $ broadcastTrx nodeApi $ x + y
      case res of
        Just it -> return it
        Nothing -> internalError

echo :: NodeApi -> Method IO
echo nodeApi = toMethod "echo" f (Required "x" :+: ())
  where
    f :: String -> RpcResult IO String
    f x = do
      res <- liftIO $ echoMsg nodeApi x
      case res of
        Just it -> return it
        Nothing -> internalError

internalError :: ExceptT RpcError IO a
internalError = throwError $ rpcError (-32603) "internal error"

{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Trans    (liftIO)
import           Data.ByteString.Lazy
import           Network.JsonRpc.Server
import           TrialChain.Node        (NodeApi (..))

apply :: ByteString -> NodeApi -> IO (Maybe ByteString)
apply req nodeApi = call (methods nodeApi) req

methods :: NodeApi -> [Method IO]
methods nodeApi = [add $ broadcastTrx nodeApi]

add :: (Integer -> IO Integer) -> Method IO
add apiMethod = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Integer -> Integer -> RpcResult IO Integer
          f x y = liftIO $ apiMethod $ x + y

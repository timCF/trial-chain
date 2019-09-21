{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Trans    (liftIO)
import           Data.ByteString.Lazy
import           Network.JsonRpc.Server

apply :: ByteString -> (Integer -> IO ()) -> IO (Maybe ByteString)
apply req newTrx = call (methods newTrx) req

methods :: (Integer -> IO ()) -> [Method IO]
methods newTrx = [add newTrx]

add :: (Integer -> IO ()) -> Method IO
add newTrx = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Integer -> Integer -> RpcResult IO Integer
          f x y = do
            let res = x + y
            liftIO $ newTrx res
            return res

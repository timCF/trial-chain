{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Control.Monad.Trans    (liftIO)
import           Data.ByteString.Lazy
import           Network.JsonRpc.Server

apply :: ByteString -> (Integer -> IO ()) -> IO (Maybe Integer) -> IO (Maybe ByteString)
apply req newTrx getter = call (methods newTrx getter) req

methods :: (Integer -> IO ()) -> IO (Maybe Integer) -> [Method IO]
methods newTrx getter = [add newTrx getter]

add :: (Integer -> IO ()) -> IO (Maybe Integer) -> Method IO
add newTrx getter = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Integer -> Integer -> RpcResult IO Integer
          f x y = do
            liftIO $ newTrx $ x + y
            Just res <- liftIO getter
            return res

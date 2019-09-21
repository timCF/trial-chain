{-# LANGUAGE OverloadedStrings #-}

module TrialChain.JsonRpc (apply) where
import           Data.ByteString.Lazy
import           Network.JsonRpc.Server

apply :: ByteString -> IO (Maybe ByteString)
apply = call methods

methods :: [Method IO]
methods = [add]

add :: Method IO
add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Integer -> Integer -> RpcResult IO Integer
          f x y = return (x + y)

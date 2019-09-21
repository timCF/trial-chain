{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Control.Monad.Trans (liftIO)
import qualified TrialChain.JsonRpc  as JsonRpc
import           Web.Scotty

main :: IO ()
main = scotty 3000 $
  post "/" $ do
    req <- body
    res <- liftIO $ JsonRpc.apply req
    case res of
      Just it -> do
        setHeader "Content-Type" "application/json"
        raw it
      Nothing ->
        return ()

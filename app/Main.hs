{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Distributed.Backend.P2P (bootstrapNonBlocking, makeNodeId)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad.Trans (liftIO)
import Crypto.Secp256k1
import Data.Maybe
import Prelude
import System.Console.CmdArgs
import qualified TrialChain.JsonRpc as JsonRpc
import qualified TrialChain.Node as Node
import Web.Scotty

data BootConfig = BootConfig
  { webPort :: Int
  , rewardAddress :: Maybe String
  , nodePort :: Int
  , knownNode :: [String]
  } deriving (Show, Data)

main :: IO ()
main = do
  config <-
    cmdArgs $
    BootConfig
      {webPort = 3000, nodePort = 4000, knownNode = [], rewardAddress = Nothing} &=
    summary "TrialChain v0.1.0.0"
  let rewardDestination = parseRewardAddress $ rewardAddress config
  let host = "127.0.0.1"
  (node, _) <-
    bootstrapNonBlocking
      host
      (show $ nodePort config)
      ((,) host)
      initRemoteTable
      (makeNodeId <$> knownNode config)
      (Node.start rewardDestination)
  scotty (webPort config) $
    post "/" $ do
      req <- body
      res <- liftIO $ JsonRpc.apply req (Node.mkNodeApi node)
      case res of
        Just it -> do
          setHeader "Content-Type" "application/json"
          raw it
        Nothing -> return ()

--
--  NOTE function have side-effects not specified in type signature (error)
--  it's bad practice, but here in main function it's ok to call it in boot time
--
parseRewardAddress :: Maybe String -> PubKey
parseRewardAddress Nothing =
  error "--rewardaddress (HEX-encoded secp256k1 public key) argument is missing"
parseRewardAddress (Just rawRewardAddress) =
  fromMaybe
    (error "--rewardaddress (HEX-encoded secp256k1 public key) argument is invalid")
    (importPubKey $ read rawRewardAddress)

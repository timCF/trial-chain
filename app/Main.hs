{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Distributed.Backend.P2P (bootstrapNonBlocking, makeNodeId)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad.Trans (liftIO)
import Crypto.Secp256k1
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
import Prelude
import System.Console.CmdArgs
import qualified TrialChain.JsonRpc as JsonRpc
import qualified TrialChain.Node as Node
import Web.Scotty

data BootConfig =
  BootConfig
    { webPort :: Int
    , webHost :: String
    , rewardAddress :: Maybe String
    , nodePort :: Int
    , knownNode :: [String]
    }
  deriving (Show, Data)

main :: IO ()
main = do
  config <-
    cmdArgs $!
    BootConfig
      { webPort = 3000
      , webHost = "127.0.0.1"
      , nodePort = 4000
      , knownNode = []
      , rewardAddress = Nothing
      } &=
    summary "TrialChain v0.1.0.0"
  let !rewardDestination = parseRewardAddress $ rewardAddress config
  let host = webHost config
  (node, processId) <-
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
      res <- liftIO $ JsonRpc.apply req (Node.mkNodeApi node processId)
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
  error "argument --rewardaddress (HEX-encoded secp256k1 public key) is missing"
parseRewardAddress (Just rawRewardAddress) =
  case Hex.decode $ pack rawRewardAddress of
    (validHex, "") ->
      fromMaybe
        (error "argument --rewardaddress (HEX-encoded secp256k1 public key) is invalid")
        (importPubKey validHex)
    (_, invalidHex) ->
      error $
      unpack $
      "argument --rewardaddress (HEX-encoded secp256k1 public key) contains invalid chunk '" <>
      invalidHex <>
      "'"

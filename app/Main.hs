{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where
import           Control.Concurrent.MVar
import           Control.Distributed.Backend.P2P  (bootstrapNonBlocking,
                                                   makeNodeId)
import           Control.Distributed.Process.Node (initRemoteTable, runProcess)
import           Control.Monad.Trans              (liftIO)
import           System.Console.CmdArgs
import qualified TrialChain.JsonRpc               as JsonRpc
import qualified TrialChain.Node                  as Node
import           Web.Scotty

data BootConfig = BootConfig {
  webPort   :: Int,
  nodePort  :: Int,
  knownNode :: [String]
} deriving (Show, Data, Typeable)

main :: IO ()
main = do
  config <- cmdArgs $
    BootConfig{
      webPort = 3000,
      nodePort = 4000,
      knownNode = []
    } &= summary "TrialChain v0.1.0.0"

  let host = "127.0.0.1"
  (node, _) <- bootstrapNonBlocking
    host
    (show $ nodePort config)
    ((,) host)
    initRemoteTable
    (makeNodeId <$> knownNode config)
    Node.start

  --
  -- TODO : properly generate node API data instance
  --
  scotty (webPort config) $
    post "/" $ do
      req <- body
      mvar <- liftIO $ newMVar Nothing
      let setter it = swapMVar mvar (Just it)
      let getter = takeMVar mvar
      let newTrx = runProcess node . Node.newTrx setter
      res <- liftIO $ JsonRpc.apply req newTrx getter
      case res of
        Just it -> do
          setHeader "Content-Type" "application/json"
          raw it
        Nothing ->
          return ()

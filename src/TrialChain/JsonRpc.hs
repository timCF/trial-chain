module TrialChain.JsonRpc where
import           Data.ByteString.Lazy

apply :: ByteString -> IO (Maybe ByteString)
apply _ = return Nothing

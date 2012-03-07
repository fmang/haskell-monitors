module System.Monitor.Network (Interface (..), getInterfaces) where

import Data.Maybe (mapMaybe)
import System.Monitor.Internal

-- | Network interface.
data Interface = Interface
    { interface :: String -- ^ Interface name.
    , rx        :: Int    -- ^ Bytes received.
    , tx        :: Int    -- ^ Bytes transmitted.
    } deriving (Eq, Show)

-- | Read @\/proc\/net\/dev@.
getInterfaces :: IO [Interface]
getInterfaces = mapMaybe iface `fmap` getSys "/proc/net/dev"
  where iface (S ifname:N r:_:_:_:_:_:_:_:N t:_) =
            Just $ Interface (init ifname) r t
        iface _ = Nothing

module System.Monitor.Network (Interface (..), getInterfaces) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as B

-- | Network interface.
data Interface = Interface
    { interface :: String -- ^ Interface name.
    , rx        :: Int    -- ^ Bytes received.
    , tx        :: Int    -- ^ Bytes transmitted.
    } deriving (Eq, Show)

parseLine :: B.ByteString -> Maybe Interface
parseLine str =
    let (ifname, rest) = B.break (== ':') (B.dropWhile (== ' ') str)
        ws = if B.null rest then [] else B.words (B.tail rest)
        nums = mapMaybe (fmap fst . B.readInt) ws
    in case nums of
        (r:_:_:_:_:_:_:_:t:_) -> Just $ Interface (B.unpack ifname) r t
        _ -> Nothing

-- | Read @\/proc\/net\/dev@.
getInterfaces :: IO [Interface]
getInterfaces = fmap (mapMaybe parseLine . B.lines) $
    B.readFile "/proc/net/dev" >>= evaluate

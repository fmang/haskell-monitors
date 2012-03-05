module System.Monitor.Memory (KiB, Memory (..), getMemory) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as B

type KiB = Int

data Memory = Memory
    { used    :: KiB
    , free    :: KiB
    , buffers :: KiB
    , cached  :: KiB
    } deriving (Eq, Show)

parse :: B.ByteString -> Memory
parse = f . mapMaybe kb . B.lines
  where f (tot:fr:buf:ca:_) = Memory (tot-fr-buf-ca) fr buf ca
        f _ = error "invalid /proc/meminfo"
        kb = fmap fst . B.readInt . B.dropWhile (== ' ') . B.dropWhile (/= ' ')

-- | Read @\/proc\/meminfo@.
getMemory :: IO Memory
getMemory = parse `fmap` B.readFile "/proc/meminfo" >>= evaluate

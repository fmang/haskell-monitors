module System.Monitor.Mem (KiB, Mem (..), getMem) where

import Control.Exception (evaluate)
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as B

type KiB = Int

data Mem = Mem
    { used    :: KiB
    , free    :: KiB
    , buffers :: KiB
    , cached  :: KiB
    } deriving (Eq, Show)

parse :: B.ByteString -> Mem
parse = f . mapMaybe kb . B.lines
  where f (tot:fr:buf:ca:_) = Mem (tot-fr-buf-ca) fr buf ca
        f _ = error "invalid /proc/meminfo"
        kb = fmap fst . B.readInt . B.dropWhile (== ' ') . B.dropWhile (/= ' ')

-- | Read @\/proc\/meminfo@.
getMem :: IO Mem
getMem = parse `fmap` B.readFile "/proc/meminfo" >>= evaluate

module System.Monitor.Memory (KiB, Memory (..), getMemory) where

import System.Monitor.Internal

type KiB = Int

data Memory = Memory
    { used    :: KiB
    , free    :: KiB
    , buffers :: KiB
    , cached  :: KiB
    } deriving (Eq, Show)

-- | Read @\/proc\/meminfo@.
getMemory :: IO Memory
getMemory = mem `fmap` getSys "/proc/meminfo"
  where mem ((_:N tot:_):(_:N fr:_):(_:N buf:_):(_:N ca:_):_) =
            Memory (tot-fr-buf-ca) fr buf ca
        mem _ = error "invalid /proc/meminfo"

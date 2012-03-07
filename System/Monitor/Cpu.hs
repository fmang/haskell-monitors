module System.Monitor.Cpu
    ( -- * Types
      Jiffy, Status (..)
    , -- * Operations
      diff, active, total
    , -- * IO
      getStatus
    ) where

import System.Monitor.Internal

type Jiffy = Int

data Status = Status
    { user   :: Jiffy
    , nice   :: Jiffy
    , system :: Jiffy
    , idle   :: Jiffy
    , ioWait :: Jiffy
    } deriving (Eq, Show)

diff :: Status -> Status -> Status
diff (Status u' n' s' i' w') (Status u n s i w) =
    Status (u'-u) (n'-n) (s'-s) (i'-i) (w'-w)

active :: Status -> Jiffy
active s = user s + nice s + system s

total :: Status -> Jiffy
total (Status u n s i w) = u + n + s + i + w

-- | Read @\/proc\/stat@.
getStatus :: IO Status
getStatus = stat `fmap` getSys "/proc/stat"
  where stat ((_:N us:N n:N sys:N idl:N io:_):_) = Status us n sys idl io
        stat _ = error "invalid /proc/stat"

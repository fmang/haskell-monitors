module System.Monitor.Cpu
    ( -- * Types
      Jiffy, Status (..)
    , -- * Operations
      diff, active, total
    , -- * IO
      getStatus
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as B

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

parse :: B.ByteString -> Status
parse = f . mapMaybe (fmap fst . B.readInt) . B.words . B.takeWhile (/= '\n')
  where f (us:n:sys:idl:io:_) = Status us n sys idl io
        f _ = error "invalid /proc/stat"

-- | Read @\/proc\/stat@.
getStatus :: IO Status
getStatus = parse `fmap` B.readFile "/proc/stat"

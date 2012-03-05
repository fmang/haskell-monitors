module System.Monitor.Cpu (Jiffy, Status (..), getStatus) where

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

parse :: B.ByteString -> Status
parse = f . mapMaybe (fmap fst . B.readInt) . B.words . B.takeWhile (/= '\n')
  where f (us:n:sys:idl:io:_) = Status us n sys idl io
        f _ = error "invalid /proc/stat"

-- | Read @\/proc\/stat@.
getStatus :: IO Status
getStatus = parse `fmap` B.readFile "/proc/stat"

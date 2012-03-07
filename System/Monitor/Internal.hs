module System.Monitor.Internal where

import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as B

data Field = S String | N Int

getSys :: FilePath -> IO [[Field]]
getSys path = parse `fmap` B.readFile path >>= evaluate
  where parse = map (map field . B.words) . B.lines
        field s = maybe (S (B.unpack s)) (N . fst) $ B.readInt s

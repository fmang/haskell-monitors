module System.Monitor.Temperature (ThermalZone, Celsius, getTemperature) where

import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Directory (doesFileExist)

type ThermalZone = Int

type Celsius = Int

-- | Read @\/sys\/class\/thermal\/thermal_zoneN\/temp@.
getTemperature :: ThermalZone -> IO (Maybe Celsius)
getTemperature zone = do
    let path = "/sys/class/thermal/thermal_zone" ++ show zone ++ "/temp"
    e <- doesFileExist path
    if not e then return Nothing else
        B.readFile path >>= evaluate . fmap ((`div` 1000) . fst) . B.readInt

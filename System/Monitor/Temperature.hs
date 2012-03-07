module System.Monitor.Temperature (ThermalZone, Celsius, getTemperature) where

import System.Directory (doesFileExist)
import System.Monitor.Internal

type ThermalZone = Int

type Celsius = Int

-- | Read @\/sys\/class\/thermal\/thermal_zoneN\/temp@.
getTemperature :: ThermalZone -> IO (Maybe Celsius)
getTemperature zone = do
    let path = "/sys/class/thermal/thermal_zone" ++ show zone ++ "/temp"
    e <- doesFileExist path
    if e then fmap (\[[N temp]] -> Just $ temp `div` 1000) (getSys path)
         else return Nothing

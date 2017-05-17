module Rehs.IO (
    newTableIO,
    updateAndReadSlotIO)where

import Rehs
import Control.Concurrent.STM

newTableIO :: IO Table
newTableIO = atomically newTable

updateAndReadSlotIO :: SlotTransaction -> Table -> IO String
updateAndReadSlotIO transaction = atomically . updateAndReadSlot transaction
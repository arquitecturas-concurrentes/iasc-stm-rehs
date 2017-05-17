------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs (
   Table,
   SlotTransaction,
   newTable,
   updateAndReadSlot,
   setTransaction,
   clearTransaction,
   readTransaction) where

import Control.Concurrent.STM

type Table = TVar String
type SlotTransaction = Table -> STM ()

newTable :: STM Table
newTable =  newTVar ""

updateAndReadSlot :: SlotTransaction -> Table -> STM String
updateAndReadSlot transaction table = transaction table >> readTVar table

setTransaction :: String -> SlotTransaction
setTransaction value = \table -> modifyTVar table (\_ -> value)

clearTransaction :: SlotTransaction
clearTransaction  = \table -> writeTVar table ""

readTransaction :: SlotTransaction
readTransaction = \table -> return ()
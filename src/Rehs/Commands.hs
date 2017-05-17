------------------------------------------------------------
-- Command parsing functions
-- This module declares functions for parsing the user input
-- and converting it into Slot Transaction - functions that
-- return an STM ().
------------------------------------------------------------

module Rehs.Commands (
    parseSlotTransactionLine) where

import Data.List.Split (splitOn)
import Rehs (SlotTransaction, setTransaction, clearTransaction, readTransaction)

type Command = [String]

parseSlotTransactionLine :: String -> SlotTransaction
parseSlotTransactionLine = parseSlotTransactionCommand . splitOn ":"

parseSlotTransactionCommand :: Command -> SlotTransaction
parseSlotTransactionCommand ["set", value] = Rehs.setTransaction value
parseSlotTransactionCommand ["clear"]      = Rehs.clearTransaction
parseSlotTransactionCommand ["read"]       = Rehs.readTransaction
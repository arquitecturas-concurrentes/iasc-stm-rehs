------------------------------------------------------------
-- Command parsing functions
-- This module declares functions for parsing the user input
-- and converting it into Slot Transaction - functions that
-- return an STM ().
------------------------------------------------------------

module Rehs.Commands (
    parseTransactionLine) where

import Data.List.Split (splitOn)
import Rehs (SchemaTransaction, setSchema, setTransaction, clearTransaction, readTransaction)

type Command = [String]

parseTransactionLine :: String -> SchemaTransaction
parseTransactionLine = parseSlotTransactionCommand . splitOn ":"

parseSlotTransactionCommand :: Command -> SchemaTransaction
parseSlotTransactionCommand ("schema":keys)     = Rehs.setSchema
parseSlotTransactionCommand ["set", key, value] = Rehs.setTransaction key value
parseSlotTransactionCommand ["read", key]       = Rehs.readTransaction key
parseSlotTransactionCommand ["clear"]           = Rehs.clearTransaction
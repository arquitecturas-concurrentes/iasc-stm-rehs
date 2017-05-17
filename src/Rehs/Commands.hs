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
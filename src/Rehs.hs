------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs (
   Schema,
   SchemaTransaction,
   newSchema,
   updateAndReadSlot,
   setSchema,
   setTransaction,
   clearTransaction,
   readTransaction) where

import Control.Concurrent.STM
import Data.Map.Strict as Map

type Schema = TVar (Map String String)
type SchemaTransaction = Schema -> STM String

newSchema :: STM Schema
newSchema = newTVar Map.empty


updateAndReadSlot :: SchemaTransaction -> Schema -> STM String
updateAndReadSlot transaction table = transaction table

setSchema :: [String] -> SchemaTransaction
setSchema keys = \schema -> writeTVar schema . Map.fromList . zip keys . repeat $ "" >> return "newSchema"

setTransaction :: String -> String -> SchemaTransaction
setTransaction key value = \schema -> modifyTVar schema (\map -> Map.insert key value map) >> return key

readTransaction :: String -> SchemaTransaction
readTransaction key = \schema -> return ""

clearTransaction :: SchemaTransaction
clearTransaction  = \schema -> modifyTVar schema clearValues >> return "clear"

clearValues :: Map String String -> Map String String
clearValues = Map.map (\_ -> "") 
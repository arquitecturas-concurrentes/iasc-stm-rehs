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
   clearAllTransaction,
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
setSchema keys = \schema -> do 
    let newMap = Map.fromList . zip keys . repeat $ ""
    writeTVar schema newMap
    return "<OK>"

setTransaction :: String -> String -> SchemaTransaction
setTransaction key value = \schema -> modifyTVar schema (\map -> Map.insert key value map) >> return value

readTransaction :: String -> SchemaTransaction
readTransaction key = \schema -> do
    map <- readTVar schema    
    return $ Map.findWithDefault "Missing slot" key map

clearTransaction :: String -> SchemaTransaction
clearTransaction key = setTransaction key ""

clearAllTransaction :: SchemaTransaction
clearAllTransaction = \schema -> modifyTVar schema clearValues >> return ""

clearValues :: Map String String -> Map String String
clearValues = Map.map (\_ -> "") 
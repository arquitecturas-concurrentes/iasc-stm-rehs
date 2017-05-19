------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs (
   Schema,
   SchemaTransaction,
   newSchema,
   setSchema,
   readTransaction,
   upcaseTransaction,
   reverseTransaction,
   setTransaction,
   clearTransaction,
   clearAllTransaction,
   showTransaction) where

import Control.Concurrent.STM
import Data.Map.Strict as Map
import Data.Char

type Schema = TVar (Map String String)
type SchemaTransaction = Schema -> STM String

newSchema :: STM Schema
newSchema = newTVar Map.empty

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
    return $ Map.findWithDefault "" key map

reverseTransaction :: String -> SchemaTransaction
reverseTransaction key = \schema -> do
    value <- readTransaction key schema
    return $ reverse value

upcaseTransaction :: String -> SchemaTransaction
upcaseTransaction key = \schema -> do
    value <- readTransaction key schema
    return $ Prelude.map toUpper value

clearTransaction :: String -> SchemaTransaction
clearTransaction key = setTransaction key ""

clearAllTransaction :: SchemaTransaction
clearAllTransaction = \schema -> modifyTVar schema clearValues >> return ""

clearValues :: Map String String -> Map String String
clearValues = Map.map (\_ -> "") 

showTransaction :: SchemaTransaction
showTransaction schema = do
    map <- readTVar schema
    return $ showStore map

showStore :: Map String String -> String
showStore map 
    | Map.null map = "Empty Schema!"
    | otherwise = (++ "\n==="). Prelude.foldl (\result (key, value) -> result ++ "\n|--> " ++ key ++ ": " ++ value) "===\nSchema:" $ tuples
    where tuples = toList map

------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs (
   Schema,
   SchemaTransaction,
   Value,
   newSchema,
   setSchema,
   readTransaction,
   upcaseTransaction,
   reverseTransaction,
   setTransaction,
   clearTransaction,
   clearAllTransaction,
   showTransaction) where

import           Control.Concurrent.STM
import           Data.Char              (toUpper)
import           Data.Map.Strict        as Map

type Key = String
type Value = Maybe String

type Store = Map Key Value
type Schema = TVar Store

type SchemaTransaction = Schema -> STM Value

newSchema :: STM Schema
newSchema = newTVar empty

setSchema :: [Key] -> SchemaTransaction
setSchema keys schema = do
    writeTVar schema . Map.fromList $ [(key, Nothing) | key <- keys]
    return $ Just "<OK>"

setTransaction :: Key -> Value -> SchemaTransaction
setTransaction key value schema = do
    modifyTVar schema $ Map.insert key value
    return value

readTransaction :: Key -> SchemaTransaction
readTransaction key schema = do
    store <- readTVar schema
    return $ Map.findWithDefault Nothing key store

mapTransaction :: ( String -> String ) -> Key -> SchemaTransaction
mapTransaction f key schema = do
    value <- readTransaction key schema
    setTransaction key (f <$> value) schema

reverseTransaction :: Key -> SchemaTransaction
reverseTransaction = mapTransaction reverse

upcaseTransaction :: Key -> SchemaTransaction
upcaseTransaction = mapTransaction $ Prelude.map toUpper

clearTransaction :: Key -> SchemaTransaction
clearTransaction key = setTransaction key Nothing

clearAllTransaction :: SchemaTransaction
clearAllTransaction schema = do
    modifyTVar schema $ Map.map (const Nothing)
    return Nothing

showTransaction :: SchemaTransaction
showTransaction schema = do
    map <- readTVar schema
    return . Just . showStore $ map

showStore :: Store -> String
showStore map
    | Map.null map = "Empty Store!"
    | otherwise = concat [header, Prelude.foldl showSlot "Store:\n" . toList $ map, footer]
    where header = "====\n"
          footer = "===="

showSlot :: String -> (Key, Value) -> String
showSlot previous (key, Nothing) = showSlot previous (key, Just "")
showSlot previous (key, Just value) = previous ++ "|---> "  ++ key ++ ": " ++ value ++ "\n"

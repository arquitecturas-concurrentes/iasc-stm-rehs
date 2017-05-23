------------------------------------------------------------
-- IO Helpers
-- This modules declares a helper functions to be able to
-- execute table transactions within IO.
-- Notice that all functions in this module return IO's
------------------------------------------------------------

module Rehs.IO (
    newSchemaIO,
    performAndReadTransactionIO)where

import           Control.Concurrent.STM
import           Data.Maybe             (fromMaybe)
import           Rehs                   (Schema, SchemaTransaction, Value,
                                         newSchema)

newSchemaIO :: IO Schema
newSchemaIO = atomically newSchema

performAndReadTransactionIO :: SchemaTransaction -> Schema -> IO String
performAndReadTransactionIO transaction = fmap (fromMaybe "") . atomically . transaction

------------------------------------------------------------
-- IO Helpers
-- This modules declares a helper functions to be able to
-- execute table transactions within IO.
-- Notice that all functions in this module return IO's
------------------------------------------------------------

module Rehs.IO (
    newSchemaIO,
    performAndReadTransactionIO)where

import Rehs
import Control.Concurrent.STM

newSchemaIO :: IO Schema
newSchemaIO = atomically newSchema

performAndReadTransactionIO :: SchemaTransaction -> Schema -> IO String
performAndReadTransactionIO transaction = atomically . updateAndReadSlot transaction
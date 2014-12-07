{-# LANGUAGE DeriveDataTypeable #-}
module Hicks.Types where

import Data.Typeable
import Data.Data (Data)
import Data.Text (Text)

data Machine = Machine
  { machineTitle :: Text
  , machineHostname :: Text
  , machineDistribution :: Distribution
  , machineProvider :: Provider
  , machinePublicKey :: FilePath
    -- ^ Path to a public SSH key uploaded to the provisioned machine at the
    -- `authorize` step.
  }
  deriving (Data, Typeable)

data Distribution =
    Ubuntu1204
  | Ubuntu1404
  deriving (Data, Typeable)

data Provider =
    UpCloud Zone
  deriving (Data, Typeable)

data Zone =
    UkLondon1
  deriving (Data, Typeable)

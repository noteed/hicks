{-# LANGUAGE OverloadedStrings #-}
-- | This is an example configuration file for Hicks. You can either modify
-- it, or create a new one in a new project.
module Main (main) where

import Hicks.CmdLine
import Hicks.Types

main :: IO ()
main = defaultMain machines

machines :: [Machine]
machines =
  [ Machine
    { machineTitle = "hicks.noteed.com (Ubuntu 14.04)"
    , machineHostname = "hicks.noteed.com"
    , machineDistribution = Ubuntu1404
    , machineProvider = UpCloud UkLondon1
    }
  ]

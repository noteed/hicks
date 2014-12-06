{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Provisioning of remote machines, as provider-agnostic as possible (so it
-- can be used on, say, UpCloud or OVH).
module Hicks.Provision where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Template
import System.Directory
  ( doesDirectoryExist, doesFileExist, getPermissions, removeFile
  , setPermissions
  )
import System.FilePath ((</>))
import System.FilePath.Find
import System.IO.Temp (withTempDirectory)
import System.Process (runProcess, waitForProcess)

upload :: String -> String -> String -> IO ()
upload ip port hostname = withTempDirectory "/tmp" "provision." $ \tempDir -> do
  h' <- runProcess "rsync"
    [ "-r"
    , "provision/" ++ hostname ++ "/"
    , tempDir
    ]
    Nothing Nothing Nothing Nothing Nothing
  _ <- waitForProcess h'
  e <- doesDirectoryExist $ "secret/" ++ hostname
  when e $ do
    h <- runProcess "rsync"
      [ "-r"
      , "secret/" ++ hostname ++ "/"
      , tempDir
      ]
      Nothing Nothing Nothing Nothing Nothing
    _ <- waitForProcess h
    replace tempDir
  writeFile (tempDir </> "HOSTNAME") hostname
  writeFile (tempDir </> "STATE") "Upload"
  h_ <- runProcess "rsync"
    [ "-re"
    , "ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "
      ++ port
    , tempDir ++ "/"
    , "root@" ++ ip ++ ":/"
    ]
    Nothing Nothing Nothing Nothing Nothing
  _ <- waitForProcess h_
  return ()

-- | Call /root/bin/provision on the target machine. The call is non-blocking
-- and stdout/stderr are directed to /root/provision.log.
provision :: String -> String -> IO ()
provision ip port = do
  h <- runProcess "ssh"
    [ "-A"
    , "-q"
    , "-o", "UserKnownHostsFile=/dev/null"
    , "-o", "StrictHostKeyChecking=no"
    , "-p", port
    , "root@" ++ ip
    , "nohup bin/provision < /dev/null > provision.log 2>&1 &"
    ]
    Nothing Nothing Nothing Nothing Nothing
  _ <- waitForProcess h
  return ()

-- | Use Data.Text.Template to fill all .holes files. The values are read from
-- the secrets.json file.
replace :: FilePath -> IO ()
replace dir = do
  b <- doesFileExist $ dir </> "secrets.json"
  if b
    then do
      content <- L.readFile $ dir </> "secrets.json"
      let Just decoded = decode' content
          secrets key = maybe "UNKNOWN" id $ parseMaybe (.: key) decoded
      files <- find always (extension ==? ".holes") dir
      mapM_ (generate secrets) files
      removeFile $ dir </> "secrets.json"
    else return ()

  where
  generate secrets from = do
    let to = reverse . drop 6 . reverse $ from
    content <- S.readFile $ from
    -- TODO Use `render`/`template` instead of `substitute`.
    let result = encodeUtf8 $ substitute (decodeUtf8 content) secrets
    L.writeFile to result
    p <- getPermissions from
    setPermissions to p
    removeFile $ from

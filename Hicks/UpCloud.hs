{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Hicks.UpCloud where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Monad (mzero)
import Data.Aeson hiding (Result)
import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.HashMap.Strict (fromList, toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (singleton)
import System.Directory
  ( copyFile, createDirectoryIfMissing, getHomeDirectory
  , getTemporaryDirectory, removeDirectoryRecursive, removeFile
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Files (readSymbolicLink)
import System.Process (readProcess, runProcess, waitForProcess)

import Network.Aeson.Client (apiDelete, apiGet, apiPost, Result(..))

import Hicks.Provision (provision, upload)
import qualified Hicks.Types as T

-- | Execute a GET agains the specified URI (e.g. `/account`) using the
-- supplied parameters.
ucGet :: FromJSON a => ByteString -> ByteString -> ByteString
  -> [(ByteString, Maybe ByteString)] -> IO (Maybe a)
ucGet username password =
  apiGet (Just (username, password)) "https://api.upcloud.com"

-- | Execute a POST agains the specified URI (e.g. `/server`) using the
-- supplied parameters.
ucPost :: FromJSON a => ByteString -> ByteString -> ByteString
  -> [(ByteString, Maybe ByteString)] -> LB.ByteString -> IO (Maybe a)
ucPost username password uri parameters body =
  apiPost (Just (username, password)) "https://api.upcloud.com" uri parameters body

-- | Execute a DELETE agains the specified URI (e.g. `/server`) using the
-- supplied parameters.
ucDelete :: FromJSON a => ByteString -> ByteString -> ByteString
  -> [(ByteString, Maybe ByteString)] -> IO (Result a)
ucDelete username password uri parameters =
  apiDelete (Just (username, password)) "https://api.upcloud.com" uri parameters

-- | Return account information.
account :: ByteString -> ByteString -> IO (Maybe Account)
account username password =
  (accountResponse <$>) <$> ucGet username password "/1.0/account" []

-- | Create a new server.
createServer :: ByteString -> ByteString -> CreateServer -> IO (Maybe CreatedServer)
createServer username password conf =
  (createServerResponse <$>) <$> ucPost username password "/1.0/server" [] body
  where
  body = encode $ createServerToJSON conf

data CreateServer = CreateServer
  { createServerTitle :: Text
  , createServerHostname :: Text
  , createServerTemplate :: StorageTemplate
  }

defaultCreateServer :: CreateServer
defaultCreateServer = CreateServer
  { createServerTitle = "Ubuntu 12.04 64-bits"
  , createServerHostname = "www.example.com"
  , createServerTemplate = Ubuntu1204
  }

createServerToJSON :: CreateServer -> Value
createServerToJSON CreateServer{..} =
  Object $ fromList [("server", Object $ fromList
    [ ("zone", String "uk-lon1")
    , ("title", String createServerTitle)
    , ("hostname", String createServerHostname)
    , ("password_delivery", String "none")
    , ("core_number", String "1")
    , ("memory_amount", String "1024")
    , ("storage_devices", Object $ fromList
      [ ("storage_device", Array $ singleton $
        Object $ fromList
          [ ("action", String "clone")
          , ("storage", String storageUuid)
          , ("size", String "10")
          , ("title", String storageTitle)
          ]
        )
      ])
    ])]

  where
  (storageUuid, storageTitle) = templateStorateTitle createServerTemplate

-- | Return servers information.
servers :: ByteString -> ByteString -> IO ([Server])
servers username password =
  (maybe [] (serverResponse' . serverResponse)) <$> ucGet username password "/1.0/server" []

server :: ByteString -> ByteString -> String -> IO (Maybe Server')
server username password uuid =
  (server'Response <$>) <$> ucGet username password (pack $ "/1.0/server/" ++ uuid) []

-- | Stop a running server.
stopServer :: ByteString -> ByteString -> String -> IO (Maybe Server)
stopServer username password uuid =
  (stopServerResponse <$>) <$> ucPost username password (pack $ "/1.0/server/" ++ uuid ++ "/stop") [] body
  where
  body = encode $ Object $ fromList [("stop_server", Object $ fromList
    [ ("stop_type", String "soft")
    , ("timeout", String "60")
    ])]

-- | Delete a stopped server.
deleteServer :: ByteString -> ByteString -> String -> IO (Result ErrorResponse)
deleteServer username password uuid =
  ucDelete username password (pack $ "/1.0/server/" ++ uuid) []

-- | Delete a storage device.
deleteStorage :: ByteString -> ByteString -> String -> IO (Result ErrorResponse)
deleteStorage username password uuid =
  ucDelete username password (pack $ "/1.0/storage/" ++ uuid) []

-- | Find a server from a given hostname (assumed to be unique). Use `server`
-- to retrieve a server from its UUID.
serverFromHostname :: ByteString -> ByteString -> String -> IO (Maybe Server')
serverFromHostname username password hostname = do
  ss <- servers username password
  case filter ((== T.pack hostname) . serverHostname) ss of
    [s] -> server username password (T.unpack $ serverUuid s)
    _ -> return Nothing

-- | Find a server from a given hostname (assumed to be unique).
serverFromHostname_ :: ByteString -> ByteString -> String -> IO (Maybe Server)
serverFromHostname_ username password hostname = do
  ss <- servers username password
  case filter ((== T.pack hostname) . serverHostname) ss of
    [s] -> return $ Just s
    _ -> return Nothing

-- | Find a server from a UUID.
serverFromUuid :: ByteString -> ByteString -> String -> IO (Maybe Server)
serverFromUuid username password uuid = do
  ss <- servers username password
  case filter ((== T.pack uuid) . serverUuid) ss of
    [s] -> return $ Just s
    _ -> return Nothing

-- | Tries to connect using SSH and the previously saved password.
-- This is useful after UpCloud reports the machine is "started" to
-- check if the SSH daemon is really ready to accept connections.
connect :: String -> IO (Maybe Bool)
connect cmdServerUuid = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  case ms of
    Nothing -> putStrLn "Cannot find server." >> return Nothing
    Just s -> do
      case filter ((== "public") . ipAccess) $ server'IpAddresses s of
        ip : _ -> do
          home <- getHomeDirectory
          let passfile = home </> ".hicks" </> T.unpack (server'Uuid s)
          Just <$> checkSshDaemon (T.unpack $ ipAddress ip) passfile
        _ -> putStrLn "No public IP address." >> return Nothing

checkSshDaemon :: String -> String -> IO Bool
checkSshDaemon ip passfile = do
  -- "DISPLAY" is set to allow the use of "SSH_ASKPASS".
  -- See also the comment in bin/hicks.hs about "SSH_ASKPASS".
  this <- readSymbolicLink "/proc/self/exe" -- getExecutablePath in base 4.6.0.0.
  h <- runProcess "setsid"
    [ "ssh", "-q", "-o", "UserKnownHostsFile=/dev/null", "-o", "StrictHostKeyChecking=no"
    , "root@" ++ ip
    , "exit"
    ] Nothing (Just [("SSH_ASKPASS", this), ("SSH_PASSWORD_FILE", passfile), ("DISPLAY", "dummy:0")])
    Nothing Nothing Nothing
  ecode <- waitForProcess h
  case ecode of
    ExitSuccess -> return True
    _ -> return False

waitSshDaemon :: String -> String -> Int -> IO ()
waitSshDaemon ip passfile n = do
  b <- checkSshDaemon ip passfile
  if b
    then return ()
    else
      if n <= 0
      then error "Can't connect to server."
      else do
        threadDelay 1000000
        waitSshDaemon ip passfile (n - 1)

authorize :: String -> FilePath -> IO ()
authorize cmdServerUuid cmdPublicKey = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  case ms of
    Nothing -> putStrLn "Cannot find server."
    Just s -> do
      case filter ((== "public") . ipAccess) $ server'IpAddresses s of
        ip : _ -> do
          home <- getHomeDirectory
          tempDir <- getTemporaryDirectory
          let temp = tempDir </> "upcloud-remote-home"
              passfile = home </> ".hicks" </> T.unpack (server'Uuid s)
          createDirectoryIfMissing True $ temp </> ".ssh"
          copyFile cmdPublicKey $ temp </> ".ssh" </> "authorized_keys"

          -- No idea why but it seems that if the SSH daemon is not up, the
          -- first attempt to connect will wait and succeed. Still the second
          -- attempt will fail. In such a case, the first line here will not
          -- enter the waiting loop but the second one will.
          waitSshDaemon (T.unpack $ ipAddress ip) passfile 30
          waitSshDaemon (T.unpack $ ipAddress ip) passfile 30
          -- "DISPLAY" is set to allow the use of "SSH_ASKPASS".
          -- See also the comment in bin/hicks.hs about "SSH_ASKPASS".
          this <- readSymbolicLink "/proc/self/exe" -- getExecutablePath in base  4.6.0.0.
          h <- runProcess "setsid"
            [ "rsync", "-re", "ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
            , temp ++ "/",  "root@" ++ T.unpack (ipAddress ip) ++ ":"
            ] Nothing (Just [("SSH_ASKPASS", this), ("SSH_PASSWORD_FILE", passfile), ("DISPLAY", "dummy:0")])
            Nothing Nothing Nothing
          _ <- waitForProcess h

          removeDirectoryRecursive temp
          removeFile passfile
          return ()
        _ -> putStrLn "No public IP address."

deploy :: [T.Machine] -> Text -> IO ()
deploy machines hostname = withAPIKey $ \u p -> do
  ms <- createServer u p $ deriveParameters machines hostname
  case ms of
    Nothing -> putStrLn "Cannot create a server."
    Just CreatedServer{..} -> do
      home <- getHomeDirectory
      createDirectoryIfMissing False $ home </> ".hicks"
      writeFile (home </> ".hicks" </> T.unpack createdServerUuid) $
        T.unpack createdServerPassword
      putStrLn $ "Server UUID is " ++ T.unpack createdServerUuid ++ "."

      b <- waitServer u p (T.unpack createdServerUuid) "started"
      if not b
        then putStrLn "Cannot wait server."
        else do
          authorize (T.unpack createdServerUuid) "provision/assertive.io/root/.ssh/authorized_keys"
          case filter ((== "public") . ipAccess) createdServerIpAddresses of
            ip : _ -> do
              upload (T.unpack $ ipAddress ip) "22" (T.unpack hostname)
              provision (T.unpack $ ipAddress ip) "22"
              return ()
            _ -> putStrLn "No public IP address."

test :: IO ()
test = withAPIKey $ \u p -> do
  putStrLn "Creating server..."
  ms <- createServer u p defaultCreateServer
  case ms of
    Nothing -> putStrLn "Cannot create server."
    Just CreatedServer{..} -> do
      b <- waitServer u p (T.unpack createdServerUuid) "started"
      if not b
        then putStrLn "Cannot wait server."
        else do
          putStrLn "Stopping server..."
          ms' <- stopServer u p $ T.unpack createdServerUuid
          case ms' of
            Nothing -> error "Cannor stop server."
            Just _ -> do
              b' <- waitServer u p (T.unpack createdServerUuid) "stopped"
              if not b'
                then putStrLn "Cannot wait server until it is stopped."
                else do
                  putStrLn "Deleting server..."
                  deleteServer u p (T.unpack createdServerUuid)
                  _ <- waitServer u p (T.unpack createdServerUuid) "deleted"
                  putStrLn "Done."

-- TODO hostname must be Text
waitServer :: ByteString -> ByteString -> String -> Text -> IO Bool
waitServer username password uuid state = do
  ms <- server username password uuid
  case ms of
    Just Server'{..} -> do
      s <- if state `elem` ["started", "stopped", "deleted", "maintenance"]
           then return server'State
           else
             if server'State == "started"
             then case filter ((== "public") . ipAccess) $ server'IpAddresses of
               ip : _ -> getSlashState (T.unpack $ ipAddress ip)
               _ -> error "No public IP address."
             else return server'State
      putStrLn $ "Server state is `" ++ T.unpack s ++ "`."
      if s == state
        then return True
        else do
          threadDelay (10 * 1000000) -- sleep 10 seconds
          waitServer username password uuid state
    _ -> do
      -- The "deleted" state doesn't actually exist.
      if state == "deleted"
        then return True
        else do
          putStrLn "Cannot retrieve expected server information."
          return False

-- | Return the state in /STATE.
getSlashState :: String -> IO Text
getSlashState ip = do
  s <- readProcess "ssh"
    [ "-q", "-o", "UserKnownHostsFile=/dev/null", "-o", "StrictHostKeyChecking=no"
    , "root@" ++ ip
    , "cat", "/STATE"
    ] ""
  case s of
    -- TODO Return a sum type.
    "Upload" -> return "Upload"
    "Provisioning" -> return "Provisioning"
    "Ready" -> return "Ready"
    _ -> return $ "Unknown: " `T.append` T.pack s

-- | Represent an account
data Account = Account
  { accountUsername :: Text
  , accountCredits :: Double
  }
  deriving Show

instance FromJSON Account where
  parseJSON (Object v) = Account <$>
    v .: "username" <*>
    v .: "credits"
  parseJSON _ = mzero

data AccountResponse = AccountResponse
  { accountResponse :: Account
  }

instance FromJSON AccountResponse where
  parseJSON (Object v) = AccountResponse <$>
    v .: "account"
  parseJSON _ = mzero

showAccount :: Account -> String
showAccount Account{..} = unlines
  [ "username: " ++ T.unpack accountUsername
  , "credits: " ++ show accountCredits
  ]

-- | Represent a server
data Server = Server
  { serverTitle :: Text
  , serverUuid :: Text
  , serverHostname :: Text
  , serverState :: Text
  }
  deriving Show

instance FromJSON Server where
  parseJSON (Object v) = Server <$>
    v .: "title" <*>
    v .: "uuid" <*>
    v .: "hostname" <*>
    v .: "state"
  parseJSON _ = mzero

-- | Represent a server
data Server' = Server'
  { server'Title :: Text
  , server'Uuid :: Text
  , server'Hostname :: Text
  , server'State :: Text
  , server'IpAddresses :: [IpAddress]
  , server'StorageDevices :: [StorageDevice]
  }
  deriving Show

instance FromJSON Server' where
  parseJSON (Object v) = Server' <$>
    v .: "title" <*>
    v .: "uuid" <*>
    v .: "hostname" <*>
    v .: "state" <*>
    (ipAddresses <$> (v .: "ip_addresses")) <*>
    (storageDevices <$> (v .: "storage_devices"))
  parseJSON _ = mzero

-- | Represent an IP address
data IpAddress = IpAddress
  { ipAccess :: Text
  , ipAddress :: Text
  }
  deriving Show

instance FromJSON IpAddress where
  parseJSON (Object v) = IpAddress <$>
    v .: "access" <*>
    v .: "address"
  parseJSON _ = mzero

-- | Represent IP addresses
data IpAddresses = IpAddresses
  { ipAddresses :: [IpAddress]
  }
  deriving Show

instance FromJSON IpAddresses where
  parseJSON (Object v) = IpAddresses <$>
    v .: "ip_address"
  parseJSON _ = mzero

-- | Represent a storage device
data StorageDevice = StorageDevice
  { deviceAddress :: Text
  , deviceStorage :: Text
  , deviceSize :: Int
  , deviceTitle :: Text
  , deviceType :: Text
  }
  deriving Show

instance FromJSON StorageDevice where
  parseJSON (Object v) = StorageDevice <$>
    v .: "address" <*>
    v .: "storage" <*>
    v .: "storage_size" <*>
    v .: "storage_title" <*>
    v .: "type"
  parseJSON _ = mzero

-- | Represent storage device
data StorageDevices = StorageDevices
  { storageDevices :: [StorageDevice]
  }
  deriving Show

instance FromJSON StorageDevices where
  parseJSON (Object v) = StorageDevices <$>
    v .: "storage_device"
  parseJSON _ = mzero

-- | Represent a server
data CreatedServer = CreatedServer
  { createdServerTitle :: Text
  , createdServerUuid :: Text
  , createdServerHostname :: Text
  , createdServerState :: Text
  , createdServerPassword :: Text
  , createdServerIpAddresses :: [IpAddress]
  }
  deriving Show

instance FromJSON CreatedServer where
  parseJSON (Object v) = CreatedServer <$>
    v .: "title" <*>
    v .: "uuid" <*>
    v .: "hostname" <*>
    v .: "state" <*>
    v .: "password" <*>
    (ipAddresses <$> (v .: "ip_addresses"))
  parseJSON _ = mzero

data CreateServerResponse = CreateServerResponse
  { createServerResponse :: CreatedServer
  }

instance FromJSON CreateServerResponse where
  parseJSON (Object v) = CreateServerResponse <$>
    v .: "server"
  parseJSON _ = mzero

data ServerResponse = ServerResponse
  { serverResponse :: ServerResponse'
  }

instance FromJSON ServerResponse where
  parseJSON (Object v) = ServerResponse <$>
    v .: "servers"
  parseJSON _ = mzero

data ServerResponse' = ServerResponse'
  { serverResponse' :: [Server]
  }

instance FromJSON ServerResponse' where
  parseJSON (Object v) = ServerResponse' <$>
    v .: "server"
  parseJSON _ = mzero

data Server'Response = Server'Response
  { server'Response :: Server'
  }

instance FromJSON Server'Response where
  parseJSON (Object v) = Server'Response <$>
    v .: "server"
  parseJSON _ = mzero

data StopServerResponse = StopServerResponse
  { stopServerResponse :: Server
  }

instance FromJSON StopServerResponse where
  parseJSON (Object v) = StopServerResponse <$>
    v .: "server"
  parseJSON _ = mzero

data ErrorDetails = ErrorDetails
  { errorCode :: Text
  , errorMessage :: Text
  }
  deriving Show

instance FromJSON ErrorDetails where
  parseJSON (Object v) = ErrorDetails <$>
    v .: "error_code" <*>
    v .: "error_message"
  parseJSON _ = mzero

data ErrorResponse = ErrorResponse
  { errorDetails :: ErrorDetails
  }
  deriving Show

instance FromJSON ErrorResponse where
  parseJSON (Object v) = ErrorResponse <$>
    v .: "error"
  parseJSON _ = mzero

data ApiResult a =
    ApiResult a
  | ApiError ErrorDetails
  deriving Show

instance FromJSON a => FromJSON (ApiResult a) where
  parseJSON (Object v)
    | [("error", value)] <- toList v = ApiError <$> parseJSON value
  parseJSON o = ApiResult <$> parseJSON o

showServer :: Server -> String
showServer Server{..} = unlines
  [ "title: " ++ T.unpack serverTitle
  , "uuid: " ++ T.unpack serverUuid
  , "hostname: " ++ T.unpack serverHostname
  , "state: " ++ T.unpack serverState
  ]

showServer' :: Server' -> String
showServer' Server'{..} = unlines
  [ "title: " ++ T.unpack server'Title
  , "uuid: " ++ T.unpack server'Uuid
  , "hostname: " ++ T.unpack server'Hostname
  , "state: " ++ T.unpack server'State
  , "ips: " ++ show server'IpAddresses
  , "devices: " ++ show server'StorageDevices
  ]

showCreatedServer :: CreatedServer -> String
showCreatedServer CreatedServer{..} = unlines
  [ "title: " ++ T.unpack createdServerTitle
  , "uuid: " ++ T.unpack createdServerUuid
  , "hostname: " ++ T.unpack createdServerHostname
  , "state: " ++ T.unpack createdServerState
  , "password: " ++ T.unpack createdServerPassword
  , "ips: " ++ show createdServerIpAddresses
  ]

withAPIKey :: (ByteString -> ByteString -> IO a) -> IO a
withAPIKey f = do
  content <- readFile "secret/upcloud-key.txt"
  let basic = head $ lines content
      username = takeWhile (/= ':') basic
      password = drop (length username + 1) basic
  f (pack username) (pack password)

deriveParameters :: [T.Machine] -> Text -> CreateServer
deriveParameters machines hostname = case lookup hostname machines' of
  Just T.Machine{..} -> CreateServer
    { createServerTitle = machineTitle
    , createServerHostname = machineHostname
    , createServerTemplate = case machineDistribution of
        T.Ubuntu1404 -> Ubuntu1404
        T.Ubuntu1204 -> Ubuntu1204
    }
  Nothing -> error "Cannot derive parameters from the given hostname."
  where
  machines' = map (\m -> (T.machineHostname m, m)) machines

data StorageTemplate =
    Ubuntu1204
  -- ^ Ubuntu Server 12.04 LTS (Precise Pangolin), 64-bit
  | Ubuntu1404
  -- ^ Ubuntu Server 14.04 LTS (Trusty Tahr)

templateStorateTitle :: StorageTemplate -> (Text, Text)
templateStorateTitle t = case t of
  Ubuntu1204 -> ("01000000-0000-4000-8000-000030030200", "ubuntu-1204")
  Ubuntu1404 -> ("01000000-0000-4000-8000-000030040200", "ubuntu-1404")

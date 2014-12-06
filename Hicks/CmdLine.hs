{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- Done:
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/account
--
-- TODO:
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/price
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/server_size
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/storage
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/storage/template
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/timezone
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/zone
--
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/server
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/server/:uuid
module Hicks.CmdLine where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_hicks (version)
import System.Console.CmdArgs.Implicit
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))

import Hicks.Provision (provision, upload)
import Hicks.UpCloud
import Hicks.Types

defaultMain :: [Machine] -> IO ()
defaultMain machines = do
  mpath <- lookup "SSH_PASSWORD_FILE" <$> getEnvironment
  case mpath of
    -- Special case when "SSH_PASSWORD_FILE" is in the environment.
    -- This program is used with the "SSH_ASKPASS" feature of SSH.
    -- The `authorize` sub-command is implemented by uploading a SSH
    -- public key to the server being provisioned. This is done using
    -- rsyn -e ssh. The problem is that ssh reads the password by
    -- using the current terminal, instead of stdin. The trick is to
    -- to use the "SSH_ASKPASS" feature, running rsync and ssh using
    -- setsid.
    -- The trick is explaied here:
    -- http://andre.frimberger.de/index.php/linux/reading-ssh-password-from-stdin-the-openssh-5-6p1-compatible-way/
    --
    Just path -> do
      content <- readFile path
      putStrLn content
    -- Regular command line interface.
    Nothing -> (runCmd =<<) $ cmdArgs $
      modes
        [ cmdAccount
        , cmdCreateServer machines
        , cmdServer
        , cmdServers
        , cmdStopServer
        , cmdDeleteServer
        , cmdUuid
        , cmdIp
        , cmdPassword
        , cmdConnect
        , cmdAuthorize
        , cmdUpload
        , cmdProvision
        , cmdDeploy machines
        , cmdWait
        , cmdTemplates machines
        ]
      &= summary versionString
      &= program "hicks"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "hicks " ++ showVersion version ++ " - Copyright (c) 2013-2014 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdAccount
  | CmdCreateServer
  { cmdServerHostname :: String
  , cmdMachines :: [Machine]
  }
  | CmdServer
  { cmdServerUuid :: String
  }
  | CmdServers
  | CmdStopServer
  { cmdServerUuid :: String
  }
  | CmdDeleteServer
  { cmdServerUuid :: String
  }
  | CmdUuid
  { cmdServerHostname :: String
  }
  | CmdIp
  { cmdServerUuid :: String
  }
  | CmdPassword
  { cmdServerUuid :: String
  }
  | CmdConnect
  { cmdServerUuid :: String
  }
  | CmdAuthorize
  { cmdServerUuid :: String
  , cmdPublicKey :: String
  }
  | CmdUpload
  { cmdServerUuid :: String
  }
  | CmdProvision
  { cmdServerUuid :: String
  }
  | CmdDeploy
  { cmdServerHostname :: String
  , cmdMachines :: [Machine]
  }
  | CmdWait
  { cmdServerUuid :: String
  , cmdState :: String
  }
  | CmdTemplates
  { cmdMachines :: [Machine]
  }
  deriving (Data, Typeable)

-- | Create an 'Account' command.
-- Similar to
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/account
cmdAccount :: Cmd
cmdAccount = CmdAccount
    &= help "Show account information."
    &= explicit
    &= name "account"

-- | Create a 'CreateServer' command.
cmdCreateServer :: [Machine] -> Cmd
cmdCreateServer machines = CmdCreateServer
  { cmdServerHostname = def
    &= argPos 0
    &= typ "HOSTNAME"
  , cmdMachines = machines
    &= ignore
  } &= help
      "Create a server. The given HOSTNAME is parsed to derive a few \
      \parameters."
    &= explicit
    &= name "create"

-- | Create a 'Servers' command.
-- Similar to
--   > curl --basic -u`cat upcloud-key.txt` https://api.upcloud.com/1.0/server
cmdServers :: Cmd
cmdServers = CmdServers
    &= help "Show servers information."
    &= explicit
    &= name "servers"

-- | Create a 'Server' command.
cmdServer :: Cmd
cmdServer = CmdServer
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER_UUID"
  } &= help "Show server information."
    &= explicit
    &= name "server"

-- | Create a 'StopServer' command.
cmdStopServer :: Cmd
cmdStopServer = CmdStopServer
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help "Stop a running server."
    &= explicit
    &= name "stop"

-- | Create a 'DeleteServer' command.
cmdDeleteServer :: Cmd
cmdDeleteServer = CmdDeleteServer
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help "Delete a stopped server (the associated disk is deleted too)."
    &= explicit
    &= name "delete"

-- | Create a 'Uuid' command.
cmdUuid :: Cmd
cmdUuid = CmdUuid
  { cmdServerHostname = def
    &= argPos 0
    &= typ "HOSTNAME"
  } &= help
      "Return the UUID of a server given its hostname."
    &= explicit
    &= name "uuid"

-- | Create an 'Ip' command.
cmdIp :: Cmd
cmdIp = CmdIp
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help
      "Return the IP address of a server."
    &= explicit
    &= name "ip"

-- | Create a 'Password' command.
cmdPassword :: Cmd
cmdPassword = CmdPassword
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help
      "Return the password of a server. The password was \
      \retrieved when the server was created and stored locally in \
      \`~/.hicks`."
    &= explicit
    &= name "password"

-- | Create a 'Connect' command.
cmdConnect :: Cmd
cmdConnect = CmdConnect
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help
      "Test if the server SSH daemon is ready to accept connections or not. \
      \This is mostly to test the `connect` function."
    &= explicit
    &= name "connect"

-- | Create an 'Authorize' command.
cmdAuthorize :: Cmd
cmdAuthorize = CmdAuthorize
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  , cmdPublicKey = def
    &= argPos 1
    &= typ "PUBLIC_KEY"
  } &= help
      "Add an SSH public key to to server's root `.ssh/authorized_keys` file. \
      \This operation also deletes the password locally saved in `~/.hicks`."
    &= explicit
    &= name "authorize"

-- | Create an 'Upload' command.
cmdUpload :: Cmd
cmdUpload = CmdUpload
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help
      "Upload a provisioning script and its associated files on the given \
      \server. The script is derived from the hostname. ssh-agent must be \
      \running."
    &= explicit
    &= name "upload"

-- | Create a 'Provision' command.
cmdProvision :: Cmd
cmdProvision = CmdProvision
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  } &= help
      "Run a provisioning script on the given server. The script is derived \
      \from the hostname. ssh-agent must be running. This is a non-blocking \
      \command."
    &= explicit
    &= name "provision"

-- | Create a 'Deploy' command.
cmdDeploy :: [Machine] -> Cmd
cmdDeploy machines = CmdDeploy
  { cmdServerHostname = def
    &= argPos 0
    &= typ "SERVER HOSTNAME"
  , cmdMachines = machines
    &= ignore
  } &= help
      "Combine all the other commands, from machine creation to complete \
      \provisioning."
    &= explicit
    &= name "deploy"

-- | Create a 'Wait' command.
cmdWait :: Cmd
cmdWait = CmdWait
  { cmdServerUuid = def
    &= argPos 0
    &= typ "SERVER UUID"
  , cmdState = def
    &= argPos 1
    &= typ "STATE"
  } &= help
      "Poll the server until its state is STATE."
    &= explicit
    &= name "wait"

-- | Create a 'Templates' command.
cmdTemplates :: [Machine] -> Cmd
cmdTemplates machines = CmdTemplates
  { cmdMachines = machines
    &= ignore
  } &= help
      "List the templates available for the `create` command."
    &= explicit
    &= name "templates"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd CmdAccount{..} = do
  ma <- withAPIKey account
  putStr $ maybe "Cannot retrieve account information.\n" showAccount ma

runCmd CmdCreateServer{..} = do
  ms <- withAPIKey $ \u p -> createServer u p $
    machineOrDie cmdMachines (T.pack cmdServerHostname)
  case ms of
    Nothing -> putStrLn "Cannot create a server."
    Just CreatedServer{..} -> do
      home <- getHomeDirectory
      createDirectoryIfMissing False $ home </> ".hicks"
      writeFile (home </> ".hicks" </> T.unpack createdServerUuid) $
        T.unpack createdServerPassword
      putStrLn $ T.unpack createdServerUuid

runCmd CmdServers{..} = do
  ms <- withAPIKey servers
  mapM_ (putStr . (++ "--\n") . showServer) ms

runCmd CmdServer{..} = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  putStr $ maybe "Cannot find server.\n" showServer' ms

runCmd CmdStopServer{..} = do
  ms' <- withAPIKey $ \u p -> stopServer u p cmdServerUuid
  putStr $ maybe "Cannot stop a server.\n" showServer ms'

runCmd CmdDeleteServer{..} = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  case ms of
    Nothing -> putStrLn "Cannot find server."
    Just Server'{..} -> do
      withAPIKey $ \u p -> do
        deleteServer u p (T.unpack server'Uuid) >>= print
        mapM_ (deleteStorage u p . T.unpack . deviceStorage) server'StorageDevices

runCmd CmdUuid{..} = do
  mu <- withAPIKey $ \u p -> (server'Uuid <$>) <$> serverFromHostname u p cmdServerHostname
  putStrLn $ maybe "Cannot find server." T.unpack mu

runCmd CmdIp{..} = do
  mips <- withAPIKey $ \u p -> (server'IpAddresses <$>) <$> server u p cmdServerUuid
  case mips of
    Nothing -> putStrLn "Cannot find server."
    Just ips -> case filter ((== "public") . ipAccess) ips of
      ip : _ -> putStrLn . T.unpack $ ipAddress ip
      _ -> putStrLn "No public IP address."

runCmd CmdPassword{..} = do
  mu <- withAPIKey $ \u p -> (server'Uuid <$>) <$> server u p cmdServerUuid
  case mu of
    Nothing -> putStrLn "Cannot find server."
    Just u -> do
      home <- getHomeDirectory
      content <- readFile (home </> ".hicks" </> T.unpack u)
      putStrLn content

runCmd CmdConnect{..} = do
  mb <- connect cmdServerUuid
  case mb of
    Nothing -> putStrLn "Can't get server IP address."
    Just True -> putStrLn "Connection succeeded."
    Just False -> putStrLn "Connection failed."

-- TODO Check the public key is actually public.
runCmd CmdAuthorize{..} = authorize cmdServerUuid cmdPublicKey

runCmd CmdUpload{..} = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  case ms of
    Nothing -> putStrLn "Cannot find server."
    Just s -> do
      case filter ((== "public") . ipAccess) $ server'IpAddresses s of
        ip : _ -> do
          upload (T.unpack $ ipAddress ip) "22" (T.unpack $ server'Hostname s)
        _ -> putStrLn "No public IP address."

runCmd CmdProvision{..} = do
  ms <- withAPIKey $ \u p -> server u p cmdServerUuid
  case ms of
    Nothing -> putStrLn "Cannot find server."
    Just s -> do
      case filter ((== "public") . ipAccess) $ server'IpAddresses s of
        ip : _ -> provision (T.unpack $ ipAddress ip) "22"
        _ -> putStrLn "No public IP address."

runCmd CmdDeploy{..} = deploy cmdMachines (T.pack cmdServerHostname)

runCmd CmdWait{..} = do
  _ <- withAPIKey $ \u p -> waitServer u p cmdServerUuid (T.pack cmdState)
  return ()

runCmd CmdTemplates{..} = do
  mapM_ (putStrLn . T.unpack . machineHostname) cmdMachines

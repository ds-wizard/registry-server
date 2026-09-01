module RegistryServer.Model.Config.ServerConfigDM where

import RegistryServer.Model.Config.ServerConfig
import Shared.Model.Config.ServerConfigDM

defaultConfig :: ServerConfig
defaultConfig =
  ServerConfig
    { general = defaultGeneral
    , database = defaultDatabase
    , s3 = defaultS3
    , sentry = defaultSentry
    , analyticalMails = defaultAnalyticalMails
    , logging = defaultLogging
    , cloud = defaultCloud
    , persistentCommand = defaultPersistentCommand
    }

defaultGeneral :: ServerConfigGeneral
defaultGeneral =
  ServerConfigGeneral
    { environment = "Production"
    , clientUrl = ""
    , serverPort = 3000
    , publicRegistrationEnabled = True
    , localeEnabled = True
    }

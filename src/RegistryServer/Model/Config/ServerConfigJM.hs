module RegistryServer.Model.Config.ServerConfigJM where

import Control.Monad
import Data.Aeson

import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigDM
import Shared.Model.Config.ServerConfigJM ()

instance FromJSON ServerConfig where
  parseJSON (Object o) = do
    general <- o .: "general"
    database <- o .:? "database" .!= defaultDatabase
    s3 <- o .:? "s3" .!= defaultS3
    sentry <- o .:? "sentry" .!= defaultSentry
    analyticalMails <- o .:? "analyticalMails" .!= defaultAnalyticalMails
    logging <- o .:? "logging" .!= defaultLogging
    cloud <- o .:? "cloud" .!= defaultCloud
    persistentCommand <- o .:? "persistentCommand" .!= defaultPersistentCommand
    return ServerConfig {..}
  parseJSON _ = mzero

instance FromJSON ServerConfigGeneral where
  parseJSON (Object o) = do
    environment <- o .:? "environment" .!= defaultGeneral.environment
    clientUrl <- o .: "clientUrl"
    serverPort <- o .:? "serverPort" .!= defaultGeneral.serverPort
    publicRegistrationEnabled <- o .:? "publicRegistrationEnabled" .!= defaultGeneral.publicRegistrationEnabled
    localeEnabled <- o .:? "localeEnabled" .!= defaultGeneral.localeEnabled
    return ServerConfigGeneral {..}
  parseJSON _ = mzero

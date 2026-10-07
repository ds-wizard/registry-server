module RegistryServer.Service.Locale.RegistryLocaleService where

import qualified Data.ByteString.Char8 as BS

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Database.DAO.Locale.RegistryLocaleDAO
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Locale.RegistryLocale
import RegistryServer.S3.Locale.RegistryLocaleS3
import RegistryServer.Service.Common
import RegistryServer.Service.Locale.Common.LocaleOfferUtil
import RegistryServer.Service.Locale.RegistryLocaleMapper

getRegistryLocales :: Maybe String -> RequestContextM [LocaleDTO]
getRegistryLocales mAppVersion = do
  checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
  locales <- findRegistryLocales
  return . fmap toDTO . selectOffered mAppVersion (.code) (.version) $ locales

getRegistryLocalePo :: String -> String -> RequestContextM BS.ByteString
getRegistryLocalePo code version = do
  checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
  locale <- findRegistryLocaleByCodeAndVersion code version
  retrieveRegistryLocalePo locale.uuid

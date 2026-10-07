module RegistryServer.Service.MailTemplateLocale.RegistryMailTemplateLocaleService where

import qualified Data.ByteString.Char8 as BS

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Database.DAO.MailTemplateLocale.RegistryMailTemplateLocaleDAO
import RegistryServer.Model.Config.ServerConfig
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale
import RegistryServer.S3.MailTemplateLocale.RegistryMailTemplateLocaleS3
import RegistryServer.Service.Common
import RegistryServer.Service.Locale.Common.LocaleOfferUtil
import RegistryServer.Service.MailTemplateLocale.RegistryMailTemplateLocaleMapper

getRegistryMailTemplateLocales :: Maybe String -> RequestContextM [LocaleDTO]
getRegistryMailTemplateLocales mAppVersion = do
  checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
  locales <- findRegistryMailTemplateLocales
  return . fmap toDTO . selectOffered mAppVersion (.code) (.version) $ locales

getRegistryMailTemplateLocalePo :: String -> String -> RequestContextM BS.ByteString
getRegistryMailTemplateLocalePo code version = do
  checkIfServerFeatureIsEnabled "Locale" (\s -> s.general.localeEnabled)
  locale <- findRegistryMailTemplateLocaleByCodeAndVersion code version
  retrieveRegistryMailTemplateLocalePo locale.uuid

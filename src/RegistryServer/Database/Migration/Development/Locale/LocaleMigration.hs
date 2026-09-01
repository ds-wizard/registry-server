module RegistryServer.Database.Migration.Development.Locale.LocaleMigration where

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.S3.Locale.LocaleS3
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.Migration.Development.Locale.Data.Locales
import Shared.Model.Locale.Locale
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Locale/Locale) started"
  deleteLocales
  insertLocale localeNl
  logInfo _CMP_MIGRATION "(Locale/Locale) ended"

runS3Migration :: RequestContextM ()
runS3Migration = do
  _ <- putLocale localeNl.uuid "wizard.json" localeNlContent
  _ <- putLocale localeNl.uuid "mail.po" localeNlContent
  return ()

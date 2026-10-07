module RegistryServer.Database.Migration.Development.Locale.LocaleMigration where

import Control.Monad (void)

import RegistryServer.Database.DAO.Locale.RegistryLocaleDAO
import RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Locale.RegistryLocale
import RegistryServer.S3.Locale.RegistryLocaleS3
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Locale/Locale) started"
  deleteRegistryLocales
  insertRegistryLocale registryLocaleDe
  logInfo _CMP_MIGRATION "(Locale/Locale) ended"

runS3Migration :: RequestContextM ()
runS3Migration = void $ putRegistryLocalePo registryLocaleDe.uuid registryLocaleDePo

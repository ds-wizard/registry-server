module RegistryServer.Database.Migration.Development.MailTemplateLocale.MailTemplateLocaleMigration where

import Control.Monad (void)

import RegistryServer.Database.DAO.MailTemplateLocale.RegistryMailTemplateLocaleDAO
import RegistryServer.Database.Migration.Development.MailTemplateLocale.Data.RegistryMailTemplateLocales
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.MailTemplateLocale.RegistryMailTemplateLocale
import RegistryServer.S3.MailTemplateLocale.RegistryMailTemplateLocaleS3
import Shared.Util.Logger

runMigration :: RequestContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(MailTemplateLocale/MailTemplateLocale) started"
  deleteRegistryMailTemplateLocales
  insertRegistryMailTemplateLocale registryMailTemplateLocaleDe
  logInfo _CMP_MIGRATION "(MailTemplateLocale/MailTemplateLocale) ended"

runS3Migration :: RequestContextM ()
runS3Migration = void $ putRegistryMailTemplateLocalePo registryMailTemplateLocaleDe.uuid registryMailTemplateLocaleDePo

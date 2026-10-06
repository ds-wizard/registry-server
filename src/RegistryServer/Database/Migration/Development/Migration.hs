module RegistryServer.Database.Migration.Development.Migration (
  runMigration,
) where

import qualified RegistryServer.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified RegistryServer.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified RegistryServer.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified RegistryServer.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified RegistryServer.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified RegistryServer.Database.Migration.Development.Publication.PublicationSchemaMigration as Publication
import qualified RegistryServer.Database.Migration.Development.User.UserMigration as User
import qualified RegistryServer.Database.Migration.Development.User.UserSchemaMigration as User
import qualified RegistryServer.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration as UserEmailLink
import RegistryServer.Model.Context.ContextMappers
import qualified Shared.Database.Migration.Development.Component.ComponentMigration as Component
import qualified Shared.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import qualified Shared.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PersistentCommand
import Shared.Util.Logger

runMigration = runRequestContextWithServerContext $ do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  Common.dropFunctions
  -- 2. Drop schema
  Component.dropTables
  Publication.dropTables
  Locale.dropTables
  PersistentCommand.dropTables
  DocumentTemplate.dropTables
  Audit.dropTables
  UserEmailLink.dropTables
  KnowledgeModelPackage.dropTables
  User.dropTables
  -- 3. Drop DB types
  Common.dropTypes
  -- 4. Create DB types
  Common.createTypes
  -- 5. Create schema
  User.createTables
  KnowledgeModelPackage.createTables
  UserEmailLink.createTables
  Audit.createTables
  DocumentTemplate.createTables
  PersistentCommand.createTables
  Locale.createTables
  Component.createTables
  Publication.createTables
  -- 6. Create DB functions
  Common.createFunctions
  -- 7. Load fixtures
  User.runMigration
  KnowledgeModelPackage.runMigration
  DocumentTemplate.runMigration
  PersistentCommand.runMigration
  Locale.runMigration
  Component.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing

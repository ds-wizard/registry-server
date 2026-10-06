module TestMigration where

import Data.Foldable (traverse_)

import RegistryServer.Database.DAO.Audit.AuditEntryDAO
import RegistryServer.Database.DAO.Publication.PublicationDAO
import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.DAO.UserToken.UserTokenDAO
import qualified RegistryServer.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified RegistryServer.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified RegistryServer.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified RegistryServer.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified RegistryServer.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified RegistryServer.Database.Migration.Development.Publication.PublicationSchemaMigration as Publication
import qualified RegistryServer.Database.Migration.Development.User.Data.Users as Users
import qualified RegistryServer.Database.Migration.Development.User.UserSchemaMigration as User
import qualified RegistryServer.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration as UserEmailLink
import qualified RegistryServer.Database.Migration.Development.UserToken.Data.UserTokens as UserTokens
import Shared.Database.DAO.Component.ComponentDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import qualified Shared.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

import Specs.Common

buildSchema requestContext =
  -- 1. Drop
  do
    putStrLn "DB: dropping schema"
    runInContext Component.dropTables requestContext
    runInContext Publication.dropTables requestContext
    runInContext Locale.dropTables requestContext
    runInContext PersistentCommand.dropTables requestContext
    runInContext UserEmailLink.dropTables requestContext
    runInContext Audit.dropTables requestContext
    runInContext User.dropTables requestContext
    runInContext KnowledgeModelPackage.dropTables requestContext
    runInContext DocumentTemplate.dropTables requestContext
    putStrLn "DB: Drop DB types"
    runInContext Common.dropTypes requestContext
    -- 2. Create
    putStrLn "DB: Create DB types"
    runInContext Common.createTypes requestContext
    putStrLn "DB: Creating schema"
    runInContext User.createTables requestContext
    runInContext KnowledgeModelPackage.createTables requestContext
    runInContext UserEmailLink.createTables requestContext
    runInContext Audit.createTables requestContext
    runInContext DocumentTemplate.createTables requestContext
    runInContext PersistentCommand.createTables requestContext
    runInContext Locale.createTables requestContext
    runInContext Component.createTables requestContext
    runInContext Publication.createTables requestContext

resetDB requestContext = do
  runInContext deletePersistentCommands requestContext
  runInContext deleteUserEmailLinks requestContext
  runInContext deleteAuditEntries requestContext
  runInContext deletePackages requestContext
  runInContext deleteDocumentTemplates requestContext
  runInContext deleteLocales requestContext
  runInContext deletePublications requestContext
  runInContext deleteUsers requestContext
  runInContext deleteComponents requestContext
  runInContext (insertUser Users.userAdmin) requestContext
  runInContext (insertUser Users.userNikola) requestContext
  runInContext (insertUserToken UserTokens.adminApiKey) requestContext
  runInContext (insertUserToken UserTokens.nikolaApiKey) requestContext
  runInContext (insertPackage globalKmPackageEmpty) requestContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEmptyEvents) requestContext
  runInContext (insertPackage globalKmPackage) requestContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEvents) requestContext
  runInContext (insertPackage netherlandsKmPackage) requestContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageEvents) requestContext
  runInContext (insertPackage netherlandsKmPackageV2) requestContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageV2Events) requestContext
  return ()

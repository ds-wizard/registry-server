module Specs.Api.Handler.Locale.Common where

import Data.Foldable (traverse_)

import RegistryServer.Database.DAO.Locale.RegistryLocaleDAO
import RegistryServer.Database.Migration.Development.Locale.Data.RegistryLocales
import qualified RegistryServer.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration

import Specs.Common

-- --------------------------------
-- FIXTURES
-- --------------------------------
runLocaleVersionsMigration requestContext = do
  runInContextIO LOC_Migration.runMigration requestContext
  runInContextIO (traverse_ insertRegistryLocale registryLocaleVersions) requestContext

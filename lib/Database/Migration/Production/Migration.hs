module Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import qualified
       Database.Migration.Production.Migration_0001_organization_init.Migration
       as M_0001
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration
import LensesConfig

runMigration baseContext = do
  migrateDatabase (baseContext ^. pool) migrationDefinitions
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions = [M_0001.definition]

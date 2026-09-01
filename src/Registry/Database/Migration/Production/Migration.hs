module Registry.Database.Migration.Production.Migration where

import Database.PostgreSQL.Migration.Entity

import qualified Registry.Database.Migration.Production.Migration_4_35_0.Migration as M_4_35_0

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions =
  [ M_4_35_0.definition
  ]

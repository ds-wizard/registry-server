module RegistryServer.Database.Migration.Production.Migration_5_0_0.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5000000, mmName = "Workspace column", mmDescription = "Add the workspace column the shared package and template rows carry"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_package ADD COLUMN workspace_uuid uuid; \
        \ALTER TABLE document_template ADD COLUMN workspace_uuid uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

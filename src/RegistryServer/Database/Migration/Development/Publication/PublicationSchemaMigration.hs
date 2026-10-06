module RegistryServer.Database.Migration.Development.Publication.PublicationSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Publication) drop tables"
  let sql = "DROP TABLE IF EXISTS publication CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Publication) create table"
  let sql =
        "CREATE TABLE publication \
        \( \
        \    entity_uuid uuid NOT NULL, \
        \    created_by  uuid NOT NULL, \
        \    CONSTRAINT publication_pk PRIMARY KEY (entity_uuid), \
        \    CONSTRAINT publication_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

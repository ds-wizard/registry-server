module RegistryServer.Database.Migration.Development.Audit.AuditSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) drop table"
  let sql = "DROP TABLE IF EXISTS audit;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Audit) create table"
  let sql =
        "CREATE TABLE audit \
        \( \
        \    type                           varchar     NOT NULL, \
        \    user_uuid                      uuid, \
        \    created_at                     timestamptz NOT NULL, \
        \    user_count                     int, \
        \    knowledge_model_package_count  int, \
        \    knowledge_model_editor_count   int, \
        \    project_count                  int, \
        \    document_template_count        int, \
        \    document_count                 int, \
        \    knowledge_model_package_reference varchar, \
        \    document_template_reference       varchar, \
        \    locale_reference                  varchar \
        \);"
  let action conn = execute_ conn sql
  runDB action

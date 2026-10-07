module RegistryServer.Database.Migration.Development.Locale.LocaleSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) drop table"
  let sql = "DROP TABLE IF EXISTS locale CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Locale) create table"
  let sql =
        "CREATE TABLE locale \
        \( \
        \    uuid       uuid        NOT NULL, \
        \    code       varchar     NOT NULL, \
        \    name       varchar     NOT NULL, \
        \    version    varchar     NOT NULL, \
        \    created_at timestamptz NOT NULL, \
        \    CONSTRAINT locale_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT locale_code_version_key UNIQUE (code, version) \
        \);"
  let action conn = execute_ conn sql
  runDB action

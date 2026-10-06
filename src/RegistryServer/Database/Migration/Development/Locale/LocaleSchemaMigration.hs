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
        \    uuid                    uuid        NOT NULL, \
        \    name                    varchar     NOT NULL, \
        \    description             varchar     NOT NULL, \
        \    code                    varchar     NOT NULL, \
        \    id                      varchar     NOT NULL, \
        \    version                 varchar     NOT NULL, \
        \    default_locale          bool        NOT NULL, \
        \    license                 varchar     NOT NULL, \
        \    readme                  varchar     NOT NULL, \
        \    recommended_app_version varchar     NOT NULL, \
        \    enabled                 bool        NOT NULL, \
        \    tenant_uuid             uuid        NOT NULL, \
        \    created_at              timestamptz NOT NULL, \
        \    updated_at              timestamptz NOT NULL, \
        \    CONSTRAINT locale_pk PRIMARY KEY (uuid) \
        \); \
        \ \
        \CREATE UNIQUE INDEX locale_id_version_uindex ON locale (id, version);"
  let action conn = execute_ conn sql
  runDB action

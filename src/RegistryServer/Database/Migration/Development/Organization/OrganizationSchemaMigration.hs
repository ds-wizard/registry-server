module RegistryServer.Database.Migration.Development.Organization.OrganizationSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) drop tables"
  let sql = "DROP TABLE IF EXISTS organization;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/Organization) create tables"
  let sql =
        "CREATE TABLE organization \
        \( \
        \    organization_id varchar     NOT NULL, \
        \    name            varchar     NOT NULL, \
        \    description     varchar     NOT NULL, \
        \    email           varchar     NOT NULL, \
        \    role            varchar     NOT NULL, \
        \    token           varchar     NOT NULL, \
        \    active          boolean     NOT NULL, \
        \    logo            varchar, \
        \    created_at      timestamptz NOT NULL, \
        \    updated_at      timestamptz NOT NULL, \
        \    CONSTRAINT organization_pk PRIMARY KEY (organization_id) \
        \); \
        \ \
        \CREATE UNIQUE INDEX organization_token_uindex ON organization (token);"
  let action conn = execute_ conn sql
  runDB action

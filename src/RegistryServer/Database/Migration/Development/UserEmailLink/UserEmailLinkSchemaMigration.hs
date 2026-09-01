module RegistryServer.Database.Migration.Development.UserEmailLink.UserEmailLinkSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/UserEmailLink) drop tables"
  let sql = "DROP TABLE IF EXISTS user_email_link CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/UserEmailLink) create table"
  let sql =
        "CREATE TABLE user_email_link \
        \( \
        \    uuid        uuid                     NOT NULL, \
        \    identity    varchar                  NOT NULL, \
        \    type        varchar                  NOT NULL, \
        \    hash        varchar                  NOT NULL, \
        \    created_at  timestamptz NOT NULL, \
        \    tenant_uuid uuid                     NOT NULL, \
        \    CONSTRAINT user_email_link_pk PRIMARY KEY (uuid, tenant_uuid), \
        \    CONSTRAINT user_email_link_identity_fk FOREIGN KEY (identity) REFERENCES organization (organization_id) ON DELETE CASCADE \
        \); \
        \ \
        \CREATE UNIQUE INDEX user_email_link_hash_uindex ON user_email_link (hash);"
  let action conn = execute_ conn sql
  runDB action

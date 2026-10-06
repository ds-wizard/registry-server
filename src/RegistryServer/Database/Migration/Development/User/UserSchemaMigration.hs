module RegistryServer.Database.Migration.Development.User.UserSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/User) drop tables"
  let sql = "DROP TABLE IF EXISTS user_token CASCADE; DROP TABLE IF EXISTS user_entity CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/User) create tables"
  let sql =
        "CREATE TABLE user_entity \
        \( \
        \    uuid          uuid        NOT NULL, \
        \    email         varchar     NOT NULL, \
        \    first_name    varchar     NOT NULL, \
        \    last_name     varchar     NOT NULL, \
        \    password_hash varchar     NOT NULL, \
        \    role          varchar     NOT NULL, \
        \    active        boolean     NOT NULL, \
        \    created_at    timestamptz NOT NULL, \
        \    updated_at    timestamptz NOT NULL, \
        \    CONSTRAINT user_entity_pk PRIMARY KEY (uuid) \
        \); \
        \CREATE UNIQUE INDEX user_entity_email_uindex ON user_entity (email); \
        \CREATE TABLE user_token \
        \( \
        \    uuid       uuid        NOT NULL, \
        \    name       varchar     NOT NULL, \
        \    type       varchar     NOT NULL, \
        \    user_uuid  uuid        NOT NULL, \
        \    value_hash varchar     NOT NULL, \
        \    expires_at timestamptz, \
        \    created_at timestamptz NOT NULL, \
        \    CONSTRAINT user_token_pk PRIMARY KEY (uuid), \
        \    CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE \
        \); \
        \CREATE UNIQUE INDEX user_token_value_hash_uindex ON user_token (value_hash); \
        \CREATE INDEX user_token_user_uuid_index ON user_token (user_uuid);"
  let action conn = execute_ conn sql
  runDB action

module RegistryServer.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) drop tables"
  let sql = "DROP TABLE IF EXISTS persistent_command CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  logInfo _CMP_MIGRATION "(Table/PersistentCommand) create table"
  let sql =
        "CREATE TABLE persistent_command \
        \( \
        \    uuid               uuid        NOT NULL, \
        \    state              varchar     NOT NULL, \
        \    component          varchar     NOT NULL, \
        \    function           varchar     NOT NULL, \
        \    body               varchar     NOT NULL, \
        \    last_error_message varchar, \
        \    attempts           int         NOT NULL, \
        \    max_attempts       int         NOT NULL, \
        \    tenant_uuid        uuid        NOT NULL, \
        \    created_by         varchar, \
        \    created_at         timestamptz NOT NULL, \
        \    updated_at         timestamptz NOT NULL, \
        \    last_trace_uuid    uuid, \
        \    CONSTRAINT persistent_command_pk PRIMARY KEY (uuid) \
        \); \
        \CREATE INDEX persistent_command_queue_idx ON persistent_command (component, created_at) WHERE state <> 'DonePersistentCommandState';"
  let action conn = execute_ conn sql
  runDB action

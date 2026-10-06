module RegistryServer.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration where

import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Util.Logger

dropTables :: RequestContextM Int64
dropTables = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackage) drop tables"
  let sql =
        "DROP TABLE IF EXISTS knowledge_model_package_event CASCADE; \
        \DROP TABLE IF EXISTS knowledge_model_package CASCADE;"
  let action conn = execute_ conn sql
  runDB action

createTables :: RequestContextM Int64
createTables = do
  createKnowledgeModelPackageTable
  createKnowledgeModelPackageEventTable

createKnowledgeModelPackageTable :: RequestContextM Int64
createKnowledgeModelPackageTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackage) create table"
  let sql =
        "CREATE TABLE knowledge_model_package \
        \( \
        \    uuid                        uuid        NOT NULL, \
        \    name                        varchar     NOT NULL, \
        \    id                          varchar     NOT NULL, \
        \    version                     varchar     NOT NULL, \
        \    metamodel_version           integer     NOT NULL, \
        \    description                 varchar     NOT NULL, \
        \    readme                      varchar     NOT NULL, \
        \    license                     varchar     NOT NULL, \
        \    previous_package_uuid       uuid, \
        \    fork_of_package_id          varchar, \
        \    merge_checkpoint_package_id varchar, \
        \    created_at                  timestamptz NOT NULL, \
        \    tenant_uuid                 uuid        NOT NULL, \
        \    phase                       varchar     NOT NULL, \
        \    non_editable                bool        NOT NULL, \
        \    public                      bool        NOT NULL, \
        \    language                    varchar     NOT NULL DEFAULT 'en', \
        \    workspace_uuid              uuid, \
        \    fork_of_package_version     varchar, \
        \    merge_checkpoint_package_version varchar, \
        \    CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid) \
        \); \
        \ \
        \CREATE INDEX knowledge_model_package_id_index ON knowledge_model_package (id); \
        \ \
        \CREATE INDEX knowledge_model_package_previous_package_id_index ON knowledge_model_package (previous_package_uuid);"
  let action conn = execute_ conn sql
  runDB action

createKnowledgeModelPackageEventTable :: RequestContextM Int64
createKnowledgeModelPackageEventTable = do
  logInfo _CMP_MIGRATION "(Table/KnowledgeModelPackageEvent) create table"
  let sql =
        "CREATE TABLE IF NOT EXISTS knowledge_model_package_event \
        \( \
        \    uuid         uuid        NOT NULL, \
        \    parent_uuid  uuid        NOT NULL, \
        \    entity_uuid  uuid        NOT NULL, \
        \    content      jsonb       NOT NULL, \
        \    package_uuid uuid        NOT NULL, \
        \    tenant_uuid  uuid        NOT NULL, \
        \    created_at   timestamptz NOT NULL, \
        \    CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (uuid, package_uuid), \
        \    CONSTRAINT knowledge_model_package_event_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package (uuid) ON DELETE CASCADE \
        \);"
  let action conn = execute_ conn sql
  runDB action

module RegistryServer.Database.Migration.Production.Migration_5_0_0.Migration (
  definition,
  assertNoDuplicateEmails,
  migrateOrganizations,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Data.Time
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

import Shared.Util.Crypto (hashSHA256)
import Shared.Util.Password
import Shared.Util.String (f'')
import Shared.Util.Uuid

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 5000000, mmName = "Workspace column", mmDescription = "Add the workspace column the shared package and template rows carry, replace organization id by a single id and replace organizations by accounts"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  assertNoDuplicateIds dbPool "knowledge_model_package" "km_id"
  assertNoDuplicateIds dbPool "document_template" "template_id"
  assertNoDuplicateIds dbPool "locale" "locale_id"
  assertNoDuplicateEmails dbPool
  addWorkspaceColumn dbPool
  replaceKnowledgeModelPackageOrganizationId dbPool
  replaceDocumentTemplateOrganizationId dbPool
  replaceLocaleOrganizationId dbPool
  createUserTables dbPool
  migrateOrganizations dbPool
  createUserEmailIndex dbPool
  replaceOrganizationReferences dbPool
  renameAuditReferences dbPool
  createPublicationTable dbPool
  dropOrganization dbPool
  return Nothing

assertNoDuplicateIds :: Pool Connection -> String -> String -> LoggingT IO ()
assertNoDuplicateIds dbPool table entityIdColumn =
  runSql dbPool . fromString $
    f''
      "DO $$ \
      \DECLARE duplicates text; \
      \BEGIN \
      \    SELECT string_agg(concat_ws(':', id, version), ', ') INTO duplicates \
      \    FROM (SELECT concat(organization_id, '.', ${entityId}) AS id, version FROM ${table} GROUP BY 1, 2 HAVING count(*) > 1) d; \
      \    IF duplicates IS NOT NULL THEN \
      \        RAISE EXCEPTION '${table} holds duplicate ids, resolve them before upgrading: %', duplicates; \
      \    END IF; \
      \END $$;"
      [("table", table), ("entityId", entityIdColumn)]

addWorkspaceColumn :: Pool Connection -> LoggingT IO ()
addWorkspaceColumn dbPool =
  runSql
    dbPool
    "ALTER TABLE knowledge_model_package ADD COLUMN workspace_uuid uuid; \
    \ALTER TABLE document_template ADD COLUMN workspace_uuid uuid;"

replaceKnowledgeModelPackageOrganizationId :: Pool Connection -> LoggingT IO ()
replaceKnowledgeModelPackageOrganizationId dbPool =
  runSql
    dbPool
    "ALTER TABLE knowledge_model_package ADD COLUMN fork_of_package_version varchar, ADD COLUMN merge_checkpoint_package_version varchar; \
    \UPDATE knowledge_model_package \
    \SET km_id = concat(organization_id, '.', km_id), \
    \    fork_of_package_id = split_part(fork_of_package_id, ':', 1) || '.' || split_part(fork_of_package_id, ':', 2), \
    \    fork_of_package_version = split_part(fork_of_package_id, ':', 3), \
    \    merge_checkpoint_package_id = split_part(merge_checkpoint_package_id, ':', 1) || '.' || split_part(merge_checkpoint_package_id, ':', 2), \
    \    merge_checkpoint_package_version = split_part(merge_checkpoint_package_id, ':', 3); \
    \DROP INDEX IF EXISTS knowledge_model_package_organization_id_km_id_index; \
    \ALTER TABLE knowledge_model_package DROP COLUMN organization_id; \
    \ALTER TABLE knowledge_model_package RENAME COLUMN km_id TO id; \
    \CREATE INDEX knowledge_model_package_id_index ON knowledge_model_package (id);"

replaceDocumentTemplateOrganizationId :: Pool Connection -> LoggingT IO ()
replaceDocumentTemplateOrganizationId dbPool =
  runSql
    dbPool
    "UPDATE document_template \
    \SET template_id = concat(organization_id, '.', template_id), \
    \    allowed_packages = (SELECT coalesce(jsonb_agg(jsonb_build_object( \
    \                                   'id', CASE WHEN rule ->> 'orgId' IS NOT NULL AND rule ->> 'kmId' IS NOT NULL THEN concat(rule ->> 'orgId', '.', rule ->> 'kmId') END, \
    \                                   'minVersion', rule -> 'minVersion', \
    \                                   'maxVersion', rule -> 'maxVersion') ORDER BY position), '[]'::jsonb) \
    \                        FROM jsonb_array_elements(allowed_packages) WITH ORDINALITY AS rules(rule, position)); \
    \DROP INDEX IF EXISTS document_template_organization_id_template_id_index; \
    \ALTER TABLE document_template DROP COLUMN organization_id; \
    \ALTER TABLE document_template RENAME COLUMN template_id TO id; \
    \CREATE INDEX document_template_id_index ON document_template (id);"

replaceLocaleOrganizationId :: Pool Connection -> LoggingT IO ()
replaceLocaleOrganizationId dbPool =
  runSql
    dbPool
    "UPDATE locale SET locale_id = concat(organization_id, '.', locale_id); \
    \DROP INDEX IF EXISTS locale_organization_id_locale_id_version_uindex; \
    \ALTER TABLE locale DROP COLUMN organization_id; \
    \ALTER TABLE locale RENAME COLUMN locale_id TO id; \
    \CREATE UNIQUE INDEX locale_id_version_uindex ON locale (id, version);"

createUserTables :: Pool Connection -> LoggingT IO ()
createUserTables dbPool =
  runSql
    dbPool
    "CREATE TABLE user_entity ( \
    \    uuid uuid NOT NULL, \
    \    email character varying NOT NULL, \
    \    first_name character varying NOT NULL, \
    \    last_name character varying NOT NULL, \
    \    password_hash character varying NOT NULL, \
    \    role character varying NOT NULL, \
    \    active boolean NOT NULL, \
    \    created_at timestamp with time zone NOT NULL, \
    \    updated_at timestamp with time zone NOT NULL, \
    \    CONSTRAINT user_entity_pk PRIMARY KEY (uuid) \
    \); \
    \CREATE TABLE user_token ( \
    \    uuid uuid NOT NULL, \
    \    name character varying NOT NULL, \
    \    type character varying NOT NULL, \
    \    user_uuid uuid NOT NULL, \
    \    value_hash character varying NOT NULL, \
    \    expires_at timestamp with time zone, \
    \    created_at timestamp with time zone NOT NULL, \
    \    CONSTRAINT user_token_pk PRIMARY KEY (uuid), \
    \    CONSTRAINT user_token_user_uuid_fk FOREIGN KEY (user_uuid) REFERENCES user_entity (uuid) ON DELETE CASCADE \
    \); \
    \CREATE UNIQUE INDEX user_token_value_hash_uindex ON user_token (value_hash); \
    \CREATE INDEX user_token_user_uuid_index ON user_token (user_uuid);"

assertNoDuplicateEmails :: Pool Connection -> LoggingT IO ()
assertNoDuplicateEmails dbPool =
  runSql
    dbPool
    "DO $$ \
    \DECLARE duplicates text; \
    \BEGIN \
    \    SELECT string_agg(email, ', ') INTO duplicates \
    \    FROM (SELECT email FROM organization GROUP BY email HAVING count(*) > 1) d; \
    \    IF duplicates IS NOT NULL THEN \
    \        RAISE EXCEPTION 'organization holds duplicate emails, resolve them before upgrading: %', duplicates; \
    \    END IF; \
    \END $$;"

migrateOrganizations :: Pool Connection -> LoggingT IO ()
migrateOrganizations dbPool =
  liftIO . withResource dbPool $ \conn -> do
    organizations <- query_ conn "SELECT email, role, token, active, created_at, updated_at FROM organization"
    traverse_ (migrateOrganization conn) organizations

migrateOrganization :: Connection -> (String, String, String, Bool, UTCTime, UTCTime) -> IO ()
migrateOrganization conn (email, role, token, active, createdAt, updatedAt) = do
  userUuid <- generateUuid
  tokenUuid <- generateUuid
  passwordHash <- generatePasswordHash token
  _ <-
    execute
      conn
      "INSERT INTO user_entity VALUES (?, ?, ?, '', ?, ?, ?, ?, ?)"
      (userUuid, email, email, passwordHash, role, active, createdAt, updatedAt)
  _ <-
    execute
      conn
      "INSERT INTO user_token VALUES (?, 'Migrated token', 'ApiKeyUserTokenType', ?, ?, NULL, now())"
      (tokenUuid, userUuid, hashSHA256 token)
  return ()

createUserEmailIndex :: Pool Connection -> LoggingT IO ()
createUserEmailIndex dbPool = runSql dbPool "CREATE UNIQUE INDEX user_entity_email_uindex ON user_entity (email);"

replaceOrganizationReferences :: Pool Connection -> LoggingT IO ()
replaceOrganizationReferences dbPool =
  runSql
    dbPool
    "ALTER TABLE audit ALTER COLUMN organization_id DROP NOT NULL; \
    \UPDATE audit a SET organization_id = (SELECT u.uuid::text FROM organization o JOIN user_entity u ON u.email = o.email WHERE o.organization_id = a.organization_id); \
    \ALTER TABLE audit ALTER COLUMN organization_id TYPE uuid USING organization_id::uuid; \
    \ALTER TABLE audit RENAME COLUMN organization_id TO user_uuid; \
    \ALTER TABLE persistent_command DROP CONSTRAINT IF EXISTS persistent_command_created_by_fk; \
    \ALTER TABLE persistent_command ALTER COLUMN created_by DROP NOT NULL; \
    \UPDATE persistent_command p SET created_by = (SELECT u.uuid::text FROM organization o JOIN user_entity u ON u.email = o.email WHERE o.organization_id = p.created_by); \
    \DELETE FROM user_email_link;"

renameAuditReferences :: Pool Connection -> LoggingT IO ()
renameAuditReferences dbPool =
  runSql
    dbPool
    "ALTER TABLE audit RENAME COLUMN knowledge_model_package_id TO knowledge_model_package_reference; \
    \ALTER TABLE audit RENAME COLUMN document_template_id TO document_template_reference; \
    \ALTER TABLE audit RENAME COLUMN locale_id TO locale_reference;"

createPublicationTable :: Pool Connection -> LoggingT IO ()
createPublicationTable dbPool =
  runSql
    dbPool
    "CREATE TABLE publication ( \
    \    entity_uuid uuid NOT NULL, \
    \    created_by uuid NOT NULL, \
    \    CONSTRAINT publication_pk PRIMARY KEY (entity_uuid), \
    \    CONSTRAINT publication_created_by_fk FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE CASCADE \
    \);"

dropOrganization :: Pool Connection -> LoggingT IO ()
dropOrganization dbPool = runSql dbPool "DROP TABLE organization;"

runSql :: Pool Connection -> Query -> LoggingT IO ()
runSql dbPool sql = do
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

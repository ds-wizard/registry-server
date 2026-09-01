module Registry.Database.Migration.Production.Migration_4_35_0.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 4035000, mmName = "Init", mmDescription = "Create the initial database schema"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  assertEmptySchema dbPool
  createTypes dbPool
  createFunctions dbPool
  createTables dbPool
  createConstraints dbPool
  createIndexes dbPool
  createForeignKeys dbPool
  return Nothing

assertEmptySchema :: Pool Connection -> LoggingT IO ()
assertEmptySchema dbPool = do
  let sql =
        "DO $$ \
        \BEGIN \
        \    IF EXISTS (SELECT 1 FROM pg_tables \
        \                WHERE schemaname = current_schema() AND tablename <> 'migration') THEN \
        \        RAISE EXCEPTION 'the init migration builds the schema from nothing and this database is not empty; an existing installation is upgraded by the upgrade script for this release, not by starting the new version'; \
        \    END IF; \
        \END $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTypes :: Pool Connection -> LoggingT IO ()
createTypes dbPool = do
  let sql =
        "CREATE TYPE sem_ver_2_tuple AS ( \
        \ major integer, \
        \ minor integer \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createFunctions :: Pool Connection -> LoggingT IO ()
createFunctions dbPool = do
  let sql =
        "CREATE FUNCTION compare_version(version_1 character varying, version_2 character varying) RETURNS character varying \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     version_order varchar; BEGIN     SELECT CASE                WHEN major_version(version_1) = major_version(version_2)                    THEN CASE                             WHEN minor_version(version_1) = minor_version(version_2)                                 THEN CASE                                          WHEN patch_version(version_1) = patch_version(version_2) THEN 'EQ'                                          WHEN patch_version(version_1) < patch_version(version_2) THEN 'LT'                                          WHEN patch_version(version_1) > patch_version(version_2) THEN 'GT'                                 END                             WHEN minor_version(version_1) < minor_version(version_2) THEN 'LT'                             WHEN minor_version(version_1) > minor_version(version_2) THEN 'GT'                    END                WHEN major_version(version_1) < major_version(version_2) THEN 'LT'                WHEN major_version(version_1) > major_version(version_2) THEN 'GT'                END     INTO version_order;     RETURN version_order; END; $$; \
        \CREATE FUNCTION major_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     major_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[1]     INTO major_version;     RETURN major_version; END; $$; \
        \CREATE FUNCTION minor_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     minor_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[2]     INTO minor_version;     RETURN minor_version; END; $$; \
        \CREATE FUNCTION patch_version(version character varying) RETURNS integer \
        \    LANGUAGE plpgsql \
        \    AS $$ DECLARE     patch_version int; BEGIN     SELECT (string_to_array(version, '.')::int[])[3]     INTO patch_version;     RETURN patch_version; END; $$;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createTables :: Pool Connection -> LoggingT IO ()
createTables dbPool = do
  let sql =
        "CREATE TABLE audit ( \
        \    type character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    user_count integer, \
        \    knowledge_model_package_count integer, \
        \    knowledge_model_editor_count integer, \
        \    project_count integer, \
        \    document_template_count integer, \
        \    document_count integer, \
        \    knowledge_model_package_id character varying, \
        \    document_template_id character varying, \
        \    locale_id character varying \
        \); \
        \CREATE TABLE component ( \
        \    name character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    built_at timestamp with time zone NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    template_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    metamodel_version sem_ver_2_tuple NOT NULL, \
        \    description character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    license character varying NOT NULL, \
        \    allowed_packages jsonb NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    updated_at timestamp with time zone DEFAULT now() NOT NULL, \
        \    phase character varying DEFAULT 'ReleasedDocumentTemplatePhase'::character varying NOT NULL, \
        \    non_editable boolean DEFAULT false NOT NULL \
        \); \
        \CREATE TABLE document_template_asset ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content_type character varying NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    file_size bigint DEFAULT 0 NOT NULL, \
        \    created_at timestamp with time zone DEFAULT now() NOT NULL, \
        \    updated_at timestamp with time zone DEFAULT now() NOT NULL \
        \); \
        \CREATE TABLE document_template_file ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    file_name character varying NOT NULL, \
        \    content character varying NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    created_at timestamp with time zone DEFAULT now() NOT NULL, \
        \    updated_at timestamp with time zone DEFAULT now() NOT NULL \
        \); \
        \CREATE TABLE document_template_format ( \
        \    document_template_uuid uuid NOT NULL, \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    icon character varying NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE document_template_format_step ( \
        \    document_template_uuid uuid NOT NULL, \
        \    format_uuid uuid NOT NULL, \
        \    \"position\" integer NOT NULL, \
        \    name character varying NOT NULL, \
        \    options jsonb NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_package ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    km_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    metamodel_version integer NOT NULL, \
        \    description character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    license character varying NOT NULL, \
        \    previous_package_uuid uuid, \
        \    fork_of_package_id character varying, \
        \    merge_checkpoint_package_id character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    phase character varying DEFAULT 'ReleasedPackagePhase'::character varying NOT NULL, \
        \    non_editable boolean DEFAULT false NOT NULL, \
        \    public boolean DEFAULT false NOT NULL, \
        \    language character varying DEFAULT 'en'::character varying NOT NULL \
        \); \
        \CREATE TABLE knowledge_model_package_event ( \
        \    uuid uuid NOT NULL, \
        \    parent_uuid uuid NOT NULL, \
        \    entity_uuid uuid NOT NULL, \
        \    content jsonb NOT NULL, \
        \    package_uuid uuid NOT NULL, \
        \    tenant_uuid uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE locale ( \
        \    uuid uuid NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    code character varying NOT NULL, \
        \    organization_id character varying NOT NULL, \
        \    locale_id character varying NOT NULL, \
        \    version character varying NOT NULL, \
        \    default_locale boolean NOT NULL, \
        \    license character varying NOT NULL, \
        \    readme character varying NOT NULL, \
        \    recommended_app_version character varying NOT NULL, \
        \    enabled boolean NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE organization ( \
        \    organization_id character varying NOT NULL, \
        \    name character varying NOT NULL, \
        \    description character varying NOT NULL, \
        \    email character varying NOT NULL, \
        \    role character varying NOT NULL, \
        \    token character varying NOT NULL, \
        \    active boolean NOT NULL, \
        \    logo character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL \
        \); \
        \CREATE TABLE persistent_command ( \
        \    uuid uuid NOT NULL, \
        \    state character varying NOT NULL, \
        \    component character varying NOT NULL, \
        \    function character varying NOT NULL, \
        \    body character varying NOT NULL, \
        \    last_error_message character varying, \
        \    attempts integer NOT NULL, \
        \    max_attempts integer NOT NULL, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL, \
        \    created_by character varying, \
        \    created_at timestamp with time zone NOT NULL, \
        \    updated_at timestamp with time zone NOT NULL, \
        \    internal boolean DEFAULT true NOT NULL, \
        \    destination character varying, \
        \    last_trace_uuid uuid \
        \); \
        \CREATE TABLE user_email_link ( \
        \    uuid uuid NOT NULL, \
        \    identity character varying, \
        \    type character varying, \
        \    hash character varying, \
        \    created_at timestamp with time zone, \
        \    tenant_uuid uuid DEFAULT '00000000-0000-0000-0000-000000000000'::uuid NOT NULL \
        \);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createConstraints :: Pool Connection -> LoggingT IO ()
createConstraints dbPool = do
  let sql =
        "ALTER TABLE ONLY component \
        \    ADD CONSTRAINT component_pk PRIMARY KEY (name); \
        \ALTER TABLE ONLY document_template_asset \
        \    ADD CONSTRAINT document_template_asset_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template_file \
        \    ADD CONSTRAINT document_template_file_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY document_template_format \
        \    ADD CONSTRAINT document_template_format_pk PRIMARY KEY (document_template_uuid, uuid); \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_pk PRIMARY KEY (document_template_uuid, format_uuid, \"position\"); \
        \ALTER TABLE ONLY document_template \
        \    ADD CONSTRAINT document_template_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY knowledge_model_package_event \
        \    ADD CONSTRAINT knowledge_model_package_event_pk PRIMARY KEY (package_uuid, uuid); \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY locale \
        \    ADD CONSTRAINT locale_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY organization \
        \    ADD CONSTRAINT organization_pk PRIMARY KEY (organization_id); \
        \ALTER TABLE ONLY persistent_command \
        \    ADD CONSTRAINT persistent_command_pk PRIMARY KEY (uuid); \
        \ALTER TABLE ONLY user_email_link \
        \    ADD CONSTRAINT user_email_link_pk PRIMARY KEY (uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createIndexes :: Pool Connection -> LoggingT IO ()
createIndexes dbPool = do
  let sql =
        "CREATE UNIQUE INDEX action_key_hash_uindex ON user_email_link USING btree (hash); \
        \CREATE UNIQUE INDEX action_key_uuid_uindex ON user_email_link USING btree (uuid); \
        \CREATE UNIQUE INDEX component_name_uindex ON component USING btree (name); \
        \CREATE UNIQUE INDEX document_template_asset_uindex ON document_template_asset USING btree (uuid); \
        \CREATE UNIQUE INDEX document_template_file_uindex ON document_template_file USING btree (uuid); \
        \CREATE UNIQUE INDEX document_template_id_uindex ON document_template USING btree (uuid); \
        \CREATE INDEX document_template_organization_id_template_id_index ON document_template USING btree (organization_id, template_id); \
        \CREATE UNIQUE INDEX locale_organization_id_locale_id_version_uindex ON locale USING btree (organization_id, locale_id, version); \
        \CREATE UNIQUE INDEX organization_organization_id_uindex ON organization USING btree (organization_id); \
        \CREATE UNIQUE INDEX organization_token_uindex ON organization USING btree (token); \
        \CREATE UNIQUE INDEX persistent_command_uuid_uindex ON persistent_command USING btree (uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

createForeignKeys :: Pool Connection -> LoggingT IO ()
createForeignKeys dbPool = do
  let sql =
        "ALTER TABLE ONLY document_template_asset \
        \    ADD CONSTRAINT document_template_asset_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_file \
        \    ADD CONSTRAINT document_template_file_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format \
        \    ADD CONSTRAINT document_template_format_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_document_template_uuid_fk FOREIGN KEY (document_template_uuid) REFERENCES document_template(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY document_template_format_step \
        \    ADD CONSTRAINT document_template_format_step_format_uuid_fk FOREIGN KEY (document_template_uuid, format_uuid) REFERENCES document_template_format(document_template_uuid, uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package_event \
        \    ADD CONSTRAINT knowledge_model_package_event_package_uuid_fk FOREIGN KEY (package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY knowledge_model_package \
        \    ADD CONSTRAINT knowledge_model_package_previous_package_uuid_fk FOREIGN KEY (previous_package_uuid) REFERENCES knowledge_model_package(uuid) ON DELETE CASCADE; \
        \ALTER TABLE ONLY persistent_command \
        \    ADD CONSTRAINT persistent_command_created_by_fk FOREIGN KEY (created_by) REFERENCES organization(organization_id);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return ()

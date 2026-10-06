module RegistryServer.Database.Mapping.Audit.AuditEntry where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

import RegistryServer.Model.Audit.AuditEntry
import RegistryServer.Model.Statistics.InstanceStatistics
import Shared.Database.Mapping.Common

instance ToRow AuditEntry where
  toRow ListPackagesAuditEntry {..} =
    [ toStringField "ListPackagesAuditEntry"
    , toField userUuid
    , toField createdAt
    , toField instanceStatistics.userCount
    , toField instanceStatistics.pkgCount
    , toField instanceStatistics.kmEditorCount
    , toField instanceStatistics.prjCount
    , toField instanceStatistics.tmlCount
    , toField instanceStatistics.docCount
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow GetKnowledgeModelBundleAuditEntry {..} =
    [ toStringField "GetKnowledgeModelBundleAuditEntry"
    , toField userUuid
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField knowledgeModelPackageReference
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    ]
  toRow GetDocumentTemplateBundleAuditEntry {..} =
    [ toStringField "GetDocumentTemplateBundleAuditEntry"
    , toField userUuid
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField documentTemplateReference
    , toField (Nothing :: Maybe String)
    ]
  toRow GetLocaleBundleAuditEntry {..} =
    [ toStringField "GetLocaleBundleAuditEntry"
    , toField userUuid
    , toField createdAt
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField (Nothing :: Maybe String)
    , toField localeReference
    ]

instance FromRow AuditEntry where
  fromRow = do
    aType <- field
    case aType of
      "ListPackagesAuditEntry" -> do
        userUuid <- field
        createdAt <- field
        userCount <- field
        pkgCount <- field
        kmEditorCount <- field
        prjCount <- field
        tmlCount <- field
        docCount <- field
        let instanceStatistics = InstanceStatistics {..}
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        return $ ListPackagesAuditEntry {..}
      "GetKnowledgeModelBundleAuditEntry" -> do
        userUuid <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        knowledgeModelPackageReference <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        return $ GetKnowledgeModelBundleAuditEntry {..}
      "GetDocumentTemplateBundleAuditEntry" -> do
        userUuid <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        documentTemplateReference <- field
        _ <- field :: RowParser (Maybe String)
        return $ GetDocumentTemplateBundleAuditEntry {..}
      "GetLocaleBundleAuditEntry" -> do
        userUuid <- field
        createdAt <- field
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        _ <- field :: RowParser (Maybe String)
        localeReference <- field
        return $ GetLocaleBundleAuditEntry {..}
      _ -> error $ "Unknown AuditEntry type: " ++ aType

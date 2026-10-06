module RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable (traverse_)
import qualified Data.UUID as U

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.S3.DocumentTemplate.DocumentTemplateS3
import RegistryServer.Service.Audit.AuditService
import RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleAcl
import RegistryServer.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (toDocumentTemplateArchive)
import RegistryServer.Service.Publication.PublicationService
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleDTO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.DocumentTemplate.DocumentTemplateSimple
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Service.DocumentTemplate.Bundle.DocumentTemplateBundleMapper (fromBundle, fromDocumentTemplateArchive, toBundle)
import Shared.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Service.DocumentTemplate.DocumentTemplateUtil
import Shared.Util.Uuid

exportBundle :: Coordinate -> RequestContextM BSL.ByteString
exportBundle documentTemplateId = do
  _ <- auditGetDocumentTemplateBundle documentTemplateId
  dt <- resolveDocumentTemplateCoordinate documentTemplateId Nothing
  formats <- findDocumentTemplateFormats dt.uuid
  files <- findFilesByDocumentTemplateUuid dt.uuid
  assets <- findAssetsByDocumentTemplateUuid dt.uuid
  assetContents <- traverse (findAsset dt.uuid) assets
  return $ toDocumentTemplateArchive (toBundle dt formats files assets) assetContents

importBundle :: BSL.ByteString -> RequestContextM DocumentTemplateSimple
importBundle contentS = do
  checkWritePermission
  case fromDocumentTemplateArchive contentS of
    Right (bundle, assetContents) -> do
      validateIdentifierFormat "id" bundle.id
      uuid <- liftIO generateUuid
      let tenantUuid = U.nil
      let dt = fromBundle bundle uuid tenantUuid Nothing
      traverse_ (\(a, content) -> putAsset dt.uuid a.uuid a.contentType content) assetContents
      insertDocumentTemplate dt
      traverse_ (insertDocumentTemplateFormat . fromFormatDTO dt.uuid tenantUuid dt.createdAt dt.updatedAt) bundle.formats
      traverse_ (insertFile . fromFileDTO dt.uuid tenantUuid dt.createdAt) bundle.files
      traverse_
        ( \(assetDto, content) ->
            insertAsset $ fromAssetDTO dt.uuid (fromIntegral . BS.length $ content) tenantUuid dt.createdAt assetDto
        )
        assetContents
      recordPublication dt.uuid
      return . toSimple $ dt
    Left error -> throwError error

-- --------------------------------
-- PRIVATE
-- --------------------------------
findAsset :: U.UUID -> DocumentTemplateAsset -> RequestContextM (DocumentTemplateAsset, BS.ByteString)
findAsset documentTemplateUuid asset = do
  content <- retrieveAsset documentTemplateUuid asset.uuid
  return (asset, content)

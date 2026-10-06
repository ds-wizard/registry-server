module RegistryServer.Service.DocumentTemplate.DocumentTemplateService where

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import RegistryServer.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.DocumentTemplateUtil
import Shared.Util.Reference

getDocumentTemplates :: [(String, String)] -> Maybe SemVer2Tuple -> RequestContextM [DocumentTemplateSimpleDTO]
getDocumentTemplates queryParams mMetamodelVersion = do
  tmls <- findDocumentTemplatesFiltered queryParams mMetamodelVersion
  return . fmap toSimpleDTO . chooseTheNewest . groupDocumentTemplates $ tmls

getDocumentTemplateByCoordinate :: Coordinate -> RequestContextM DocumentTemplateDetailDTO
getDocumentTemplateByCoordinate coordinate = do
  tml <- findDocumentTemplateByCoordinate coordinate Nothing
  versions <- getDocumentTemplateVersions tml
  return $ toDetailDTO tml versions

-- --------------------------------
-- PRIVATE
-- --------------------------------
getDocumentTemplateVersions :: DocumentTemplate -> RequestContextM [String]
getDocumentTemplateVersions tml = do
  allTmls <- findDocumentTemplatesById tml.id Nothing
  return . fmap (.version) $ allTmls

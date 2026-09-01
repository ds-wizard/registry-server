module RegistryServer.Service.DocumentTemplate.DocumentTemplateService where

import RegistryPublic.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryServer.Api.Resource.DocumentTemplate.DocumentTemplateDetailDTO
import RegistryServer.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.DocumentTemplate.DocumentTemplateMapper
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO hiding (findDocumentTemplatesFiltered)
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.Coordinate.Coordinate
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Service.DocumentTemplate.DocumentTemplateUtil
import Shared.Util.Coordinate

getDocumentTemplates :: [(String, String)] -> Maybe SemVer2Tuple -> RequestContextM [DocumentTemplateSimpleDTO]
getDocumentTemplates queryParams mMetamodelVersion = do
  tmls <- findDocumentTemplatesFiltered queryParams mMetamodelVersion
  orgs <- findOrganizations
  return . fmap (toSimpleDTO orgs) . chooseTheNewest . groupDocumentTemplates $ tmls

getDocumentTemplateByCoordinate :: Coordinate -> RequestContextM DocumentTemplateDetailDTO
getDocumentTemplateByCoordinate coordinate = do
  tml <- findDocumentTemplateByCoordinate coordinate
  versions <- getDocumentTemplateVersions tml
  org <- findOrganizationByOrgId tml.organizationId
  return $ toDetailDTO tml versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getDocumentTemplateVersions :: DocumentTemplate -> RequestContextM [String]
getDocumentTemplateVersions tml = do
  allTmls <- findDocumentTemplatesByOrganizationIdAndKmId tml.organizationId tml.templateId
  return . fmap (.version) $ allTmls

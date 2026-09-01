module RegistryServer.Service.Locale.LocaleService where

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import RegistryServer.Database.DAO.Locale.LocaleDAO
import RegistryServer.Database.DAO.Organization.OrganizationDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Locale.LocaleMapper
import RegistryServer.Service.Locale.LocaleUtil
import RegistryServer.Service.Locale.LocaleValidation
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Locale.Locale
import Shared.Util.Coordinate

getLocales :: [(String, String)] -> Maybe String -> RequestContextM [LocaleDTO]
getLocales queryParams mRecommendedAppVersion = do
  checkIfLocaleEnabled
  locales <- findLocalesFiltered queryParams mRecommendedAppVersion
  orgs <- findOrganizations
  return . fmap (toDTO orgs) . chooseTheNewest . groupLocales $ locales

getLocaleByCoordinate :: Coordinate -> RequestContextM LocaleDetailDTO
getLocaleByCoordinate lId = do
  checkIfLocaleEnabled
  locale <- findLocaleByCoordinate lId
  versions <- getLocaleVersions locale
  org <- findOrganizationByOrgId locale.organizationId
  return $ toDetailDTO locale versions org

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> RequestContextM [String]
getLocaleVersions locale = do
  allTmls <- findLocalesByOrganizationIdAndLocaleId locale.organizationId locale.localeId
  return . fmap (.version) $ allTmls

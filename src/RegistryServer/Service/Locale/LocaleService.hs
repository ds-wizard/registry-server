module RegistryServer.Service.Locale.LocaleService where

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Api.Resource.Locale.LocaleDetailDTO
import RegistryServer.Database.DAO.Locale.LocaleDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Locale.LocaleMapper
import RegistryServer.Service.Locale.LocaleUtil
import RegistryServer.Service.Locale.LocaleValidation
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Locale.Locale
import Shared.Util.Reference

getLocales :: [(String, String)] -> Maybe String -> RequestContextM [LocaleDTO]
getLocales queryParams mRecommendedAppVersion = do
  checkIfLocaleEnabled
  locales <- findLocalesFiltered queryParams mRecommendedAppVersion
  return . fmap toDTO . chooseTheNewest . groupLocales $ locales

getLocaleByCoordinate :: Coordinate -> RequestContextM LocaleDetailDTO
getLocaleByCoordinate lId = do
  checkIfLocaleEnabled
  locale <- findLocaleByCoordinate lId
  versions <- getLocaleVersions locale
  return $ toDetailDTO locale versions

-- --------------------------------
-- PRIVATE
-- --------------------------------
getLocaleVersions :: Locale -> RequestContextM [String]
getLocaleVersions locale = do
  allTmls <- findLocalesById locale.id
  return . fmap (.version) $ allTmls

module RegistryServer.Database.DAO.Locale.LocaleDAO where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Locale.Locale ()
import Shared.Model.Locale.Locale
import Shared.Util.String

entityName = "locale"

findLocalesFiltered :: [(String, String)] -> Maybe String -> RequestContextM [Locale]
findLocalesFiltered queryParams mRecommendedAppVersion = do
  tenantUuid <- asks (.tenantUuid')
  let queryParamCondition = mapToDBQuerySql (tenantQueryUuid tenantUuid : queryParams)
  let recommendedAppVersionCondition =
        case mRecommendedAppVersion of
          Just _ -> "AND (compare_version(recommended_app_version, ?) = 'LT' OR compare_version(recommended_app_version, ?) = 'EQ')"
          Nothing -> ""
  let sql = fromString $ f' "SELECT * FROM locale WHERE %s %s" [queryParamCondition, recommendedAppVersionCondition]
  let params = [U.toString tenantUuid] ++ fmap snd queryParams ++ maybeToList mRecommendedAppVersion ++ maybeToList mRecommendedAppVersion
  logQuery sql params
  let action conn = query conn sql params
  runDB action

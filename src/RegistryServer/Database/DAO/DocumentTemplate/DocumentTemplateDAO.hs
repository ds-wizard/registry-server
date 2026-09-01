module RegistryServer.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Common.SemVer2Tuple ()
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.Model.Common.SemVer2Tuple
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Util.String

entityName = "document_template"

findDocumentTemplatesFiltered :: [(String, String)] -> Maybe SemVer2Tuple -> RequestContextM [DocumentTemplate]
findDocumentTemplatesFiltered queryParams mMetamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let queryParamCondition = mapToDBQuerySql (tenantQueryUuid tenantUuid : queryParams)
  let (metamodelVersionCondition, metamodelVersionParam) =
        case mMetamodelVersion of
          Just metamodelVersion ->
            ( "AND ((metamodel_version).major < ? OR ((metamodel_version).major = ? AND (metamodel_version).minor <= ?))"
            , [toField metamodelVersion.major, toField metamodelVersion.major, toField metamodelVersion.minor]
            )
          Nothing -> ("", [])
  let sql = fromString $ f' "SELECT * FROM document_template WHERE %s %s" [queryParamCondition, metamodelVersionCondition]
  let params = toField tenantUuid : fmap (toField . snd) queryParams ++ metamodelVersionParam
  logQuery sql params
  let action conn = query conn sql params
  runDB action

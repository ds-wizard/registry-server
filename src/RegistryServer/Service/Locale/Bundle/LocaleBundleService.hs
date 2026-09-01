module RegistryServer.Service.Locale.Bundle.LocaleBundleService where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.UUID as U

import RegistryPublic.Api.Resource.Locale.LocaleDTO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.S3.Locale.LocaleS3
import RegistryServer.Service.Audit.AuditService
import RegistryServer.Service.Locale.Bundle.LocaleBundleAcl
import RegistryServer.Service.Locale.LocaleMapper
import Shared.Database.DAO.Locale.LocaleDAO
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Locale.Locale
import Shared.Service.Locale.Bundle.LocaleBundleMapper
import Shared.Util.Uuid

exportBundle :: Coordinate -> RequestContextM BSL.ByteString
exportBundle lId = do
  _ <- auditGetLocaleBundle lId
  locale <- findLocaleByCoordinate lId
  wizardTranslation <- retrieveLocale locale.uuid "wizard.json"
  mailTranslation <- retrieveLocale locale.uuid "mail.po"
  return $ toLocaleArchive locale wizardTranslation mailTranslation

importBundle :: BSL.ByteString -> RequestContextM LocaleDTO
importBundle contentS = do
  checkWritePermission
  case fromLocaleArchive contentS of
    Right (bundle, wizardTranslation, mailTranslation) -> do
      uuid <- liftIO generateUuid
      let locale = fromLocaleBundle bundle uuid U.nil
      putLocale locale.uuid "wizard.json" wizardTranslation
      putLocale locale.uuid "mail.po" mailTranslation
      insertLocale locale
      return . toDTO [] $ locale
    Left error -> throwError error

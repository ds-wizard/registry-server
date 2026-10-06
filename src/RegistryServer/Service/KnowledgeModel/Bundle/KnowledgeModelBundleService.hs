module RegistryServer.Service.KnowledgeModel.Bundle.KnowledgeModelBundleService (
  exportBundle,
  importBundle,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.UUID as U

import RegistryServer.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackageRaw ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import qualified RegistryServer.Model.KnowledgeModel.Bundle.KnowledgeModelBundle as R_KnowledgeModelBundle
import RegistryServer.Model.KnowledgeModel.Package.KnowledgeModelPackageRaw
import RegistryServer.Service.Audit.AuditService
import RegistryServer.Service.KnowledgeModel.Bundle.KnowledgeModelBundleAcl
import RegistryServer.Service.Publication.PublicationService
import Shared.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageJM ()
import Shared.Constant.KnowledgeModel
import Shared.Constant.Tenant
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Localization.Messages.KnowledgeModel.Public
import Shared.Localization.Messages.Public
import Shared.Model.Coordinate.Coordinate
import Shared.Model.Error.Error
import qualified Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundle as S_KnowledgeModelBundle
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.Coordinate.CoordinateValidation
import qualified Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Shared.Util.List
import Shared.Util.Uuid

exportBundle :: Coordinate -> RequestContextM R_KnowledgeModelBundle.KnowledgeModelBundle
exportBundle coordinate = do
  _ <- auditGetKnowledgeModelBundle coordinate
  resolvedPb <- resolvePackageCoordinate coordinate Nothing
  packages <- findSeriesOfPackagesRecursiveByUuid resolvedPb.uuid
  case lastSafe packages of
    Just newestPackage -> do
      let pb =
            R_KnowledgeModelBundle.KnowledgeModelBundle
              { name = newestPackage.name
              , id = newestPackage.id
              , version = newestPackage.version
              , metamodelVersion = knowledgeModelMetamodelVersion
              , packages = packages
              }
      return pb
    Nothing -> throwError . NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "knowledge_model_package" [("tenant_uuid", U.toString defaultTenantUuid), ("uuid", U.toString resolvedPb.uuid)]

importBundle :: S_KnowledgeModelBundle.KnowledgeModelBundle -> RequestContextM S_KnowledgeModelBundle.KnowledgeModelBundle
importBundle pb =
  runInTransaction $ do
    checkWritePermission
    pkg <- extractMainPackage pb
    traverse_ (validateIdentifierFormat "id" . (.id)) pb.packages
    traverse_ importPackage pb.packages
    return pb
  where
    extractMainPackage pb =
      case L.find (\p -> createCoordinate p == createCoordinate pb) pb.packages of
        Just pkg -> return pkg
        Nothing -> throwError . UserError $ _ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE

-- --------------------------------
-- PRIVATE
-- --------------------------------
importPackage :: KnowledgeModelBundlePackage -> RequestContextM ()
importPackage dto =
  runInTransaction $ do
    eitherPackage <- findPackageByCoordinate' (createCoordinate dto) Nothing
    case eitherPackage of
      Nothing -> do
        pkgUuid <- liftIO generateUuid
        mPreviousPackageUuid <-
          case dto.previousPackageId of
            Just previousPackageId -> do
              previousPackage <- findPackageByCoordinate previousPackageId Nothing
              return . Just $ previousPackage.uuid
            Nothing -> return Nothing
        let (pkg, pkgEvents) = PM.fromKnowledgeModelBundlePackage dto pkgUuid mPreviousPackageUuid U.nil Nothing
        insertPackage pkg
        traverse_ insertPackageEvent pkgEvents
        recordPublication pkg.uuid
      Just _ -> return ()

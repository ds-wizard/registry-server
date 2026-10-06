module RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageService (
  getSimplePackagesFiltered,
  getPackageByCoordinate,
) where

import qualified Data.List as L

import RegistryPublic.Api.Resource.Package.KnowledgeModelPackageSimpleDTO
import RegistryServer.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import RegistryServer.Database.DAO.Common
import RegistryServer.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Audit.AuditService
import RegistryServer.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO hiding (findPackagesFiltered)
import Shared.Model.Coordinate.Coordinate
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageUtil
import Shared.Util.Reference

getSimplePackagesFiltered :: [(String, String)] -> Maybe Int -> [(String, String)] -> RequestContextM [KnowledgeModelPackageSimpleDTO]
getSimplePackagesFiltered queryParams mMetamodelVersion headers =
  runInTransaction $ do
    _ <- auditListPackages headers
    pkgs <- findPackagesFiltered queryParams mMetamodelVersion
    return . fmap toSimpleDTO . chooseTheNewest . groupPackages $ pkgs

getPackageByCoordinate :: Coordinate -> RequestContextM KnowledgeModelPackageDetailDTO
getPackageByCoordinate coordinate = do
  pkg <- resolvePackageCoordinate coordinate Nothing
  versions <- getPackageVersions pkg
  return $ toDetailDTO pkg versions

-- --------------------------------
-- PRIVATE
-- --------------------------------
getPackageVersions :: KnowledgeModelPackage -> RequestContextM [String]
getPackageVersions pkg = do
  allPkgs <- findPackagesById pkg.id Nothing
  return . L.sort . fmap (.version) $ allPkgs

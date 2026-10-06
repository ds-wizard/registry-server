module RegistryServer.Database.DAO.Publication.PublicationDAO where

import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Int

import RegistryServer.Database.DAO.Common
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext

entityName = "publication"

findPublicationCreatedBy :: U.UUID -> RequestContextM (Maybe U.UUID)
findPublicationCreatedBy entityUuid = do
  let sql = fromString "SELECT created_by FROM publication WHERE entity_uuid = ?"
  let params = [entityUuid]
  logQuery sql params
  let action conn = query conn sql params
  rows <- runDB action
  return $ case rows of
    [Only createdBy] -> Just createdBy
    _ -> Nothing

insertPublication :: U.UUID -> U.UUID -> RequestContextM Int64
insertPublication entityUuid createdBy = do
  let sql = fromString "INSERT INTO publication (entity_uuid, created_by) VALUES (?, ?)"
  let params = [entityUuid, createdBy]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deletePublications :: RequestContextM Int64
deletePublications = createDeleteEntitiesFn entityName

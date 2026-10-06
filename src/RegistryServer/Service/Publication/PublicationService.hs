module RegistryServer.Service.Publication.PublicationService where

import qualified Data.UUID as U

import RegistryServer.Database.DAO.Publication.PublicationDAO
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.Context.RequestContextHelpers
import RegistryServer.Model.User.User

recordPublication :: U.UUID -> RequestContextM ()
recordPublication entityUuid = do
  user <- getCurrentUser
  _ <- insertPublication entityUuid user.uuid
  return ()

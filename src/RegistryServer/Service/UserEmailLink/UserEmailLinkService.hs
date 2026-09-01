module RegistryServer.Service.UserEmailLink.UserEmailLinkService where

import Control.Monad.Reader (liftIO)
import Data.Time
import qualified Data.UUID as U

import RegistryServer.Database.Mapping.UserEmailLink.UserEmailLinkType ()
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.UserEmailLink.UserEmailLinkType
import Shared.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.Model.UserEmailLink.UserEmailLink
import Shared.Util.Uuid

createUserEmailLink :: String -> UserEmailLinkType -> RequestContextM (UserEmailLink String UserEmailLinkType)
createUserEmailLink orgId actionType = do
  uuid <- liftIO generateUuid
  hash <- liftIO generateUuid
  now <- liftIO getCurrentTime
  let userEmailLink =
        UserEmailLink
          { uuid = uuid
          , identity = orgId
          , aType = actionType
          , hash = U.toString hash
          , tenantUuid = U.nil
          , createdAt = now
          }
  insertUserEmailLink userEmailLink
  return userEmailLink

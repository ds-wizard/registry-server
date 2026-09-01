module RegistryServer.Service.Organization.OrganizationCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import RegistryPublic.Api.Resource.Organization.OrganizationCreateDTO
import RegistryPublic.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Service.Organization.OrganizationService
import Shared.Model.Error.Error
import Shared.Model.PersistentCommand.PersistentCommand
import Shared.Util.Logger

cComponent = "organization"

execute :: PersistentCommand String -> RequestContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateOrganizationName = cCreateOrganization command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateOrganizationName = "createOrganization"

cCreateOrganization :: PersistentCommand String -> RequestContextM (PersistentCommandState, Maybe String)
cCreateOrganization persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String OrganizationCreateDTO
  case eCommand of
    Right command -> do
      createOrganization command Nothing
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])

module RegistryServer.Api.Handler.UserEmailLink.Api where

import Servant
import Servant.Swagger.Tags

import RegistryServer.Api.Handler.UserEmailLink.List_POST
import RegistryServer.Model.Context.ServerContext

type UserEmailLinkAPI =
  Tags "User Email Link"
    :> List_POST

userEmailLinkApi :: Proxy UserEmailLinkAPI
userEmailLinkApi = Proxy

userEmailLinkServer :: ServerT UserEmailLinkAPI ServerContextM
userEmailLinkServer = list_POST

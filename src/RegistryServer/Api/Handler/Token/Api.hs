module RegistryServer.Api.Handler.Token.Api where

import Servant
import Servant.Swagger.Tags

import RegistryServer.Api.Handler.Token.List_POST
import RegistryServer.Model.Context.ServerContext

type TokenAPI =
  Tags "Token"
    :> List_POST

tokenApi :: Proxy TokenAPI
tokenApi = Proxy

tokenServer :: ServerT TokenAPI ServerContextM
tokenServer =
  list_POST

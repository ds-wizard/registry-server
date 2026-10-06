module RegistryServer.Api.Handler.PersistentCommand.Api where

import Servant
import Servant.Swagger.Tags

import RegistryPublic.Api.Handler.PersistentCommand.List_POST
import RegistryServer.Api.Handler.PersistentCommand.List_POST
import RegistryServer.Model.Context.ServerContext

type PersistentCommandAPI =
  Tags "Persistent Command"
    :> List_POST

persistentCommandApi :: Proxy PersistentCommandAPI
persistentCommandApi = Proxy

persistentCommandServer :: ServerT PersistentCommandAPI ServerContextM
persistentCommandServer = list_POST

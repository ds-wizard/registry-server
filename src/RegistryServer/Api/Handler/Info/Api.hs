module RegistryServer.Api.Handler.Info.Api where

import Servant

import RegistryServer.Api.Handler.Info.List_GET
import RegistryServer.Model.Context.ServerContext

type InfoAPI = List_GET

infoApi :: Proxy InfoAPI
infoApi = Proxy

infoServer :: ServerT InfoAPI ServerContextM
infoServer = list_GET

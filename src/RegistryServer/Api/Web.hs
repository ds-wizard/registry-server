module RegistryServer.Api.Web where

import Servant hiding (ServerContext)

import RegistryServer.Api.Handler.Api
import RegistryServer.Api.Handler.Swagger.Api
import RegistryServer.Model.Context.ServerContext
import Shared.Bootstrap.Web
import Shared.Model.Config.BuildInfoConfig

type WebAPI =
  SwaggerAPI
    :<|> ApplicationAPI

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: ServerContext -> Server WebAPI
webServer serverContext =
  swaggerServer serverContext.buildInfoConfig.releaseVersion :<|> hoistServer applicationApi (convert serverContext runServerContextM) applicationServer

module RegistryServer.Api.Web where

import Servant hiding (ServerContext)

import RegistryServer.Api.Handler.Api
import RegistryServer.Api.Handler.Swagger.Api
import RegistryServer.Model.Context.ContextLenses ()
import RegistryServer.Model.Context.ServerContext
import Shared.Api.Handler.Root.Api
import Shared.Bootstrap.Web
import Shared.Model.Config.BuildInfoConfig

type WebAPI = RootAPI :<|> ("api" :> (SwaggerAPI :<|> ApplicationAPI))

webApi :: Proxy WebAPI
webApi = Proxy

webServer :: ServerContext -> Server WebAPI
webServer serverContext =
  hoistServer rootApi run rootServer
    :<|> (swaggerServer version :<|> hoistServer applicationApi run applicationServer)
  where
    run :: ServerContextM a -> Handler a
    run = convert serverContext (.runServerContextM)
    version = serverContext.buildInfoConfig.releaseVersion

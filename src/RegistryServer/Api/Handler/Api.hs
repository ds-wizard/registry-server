module RegistryServer.Api.Handler.Api where

import Servant

import RegistryServer.Api.Handler.ApiKey.Api
import RegistryServer.Api.Handler.Bootstrap.Api
import RegistryServer.Api.Handler.DocumentTemplate.Api
import RegistryServer.Api.Handler.Info.Api
import RegistryServer.Api.Handler.KnowledgeModelPackage.Api
import RegistryServer.Api.Handler.Locale.Api
import RegistryServer.Api.Handler.PersistentCommand.Api
import RegistryServer.Api.Handler.Token.Api
import RegistryServer.Api.Handler.User.Api
import RegistryServer.Api.Handler.UserEmailLink.Api
import RegistryServer.Model.Context.ServerContext

type ApplicationAPI =
  InfoAPI
    :<|> ApiKeyAPI
    :<|> BootstrapAPI
    :<|> DocumentTemplateAPI
    :<|> KnowledgeModelPackageAPI
    :<|> LocaleAPI
    :<|> PersistentCommandAPI
    :<|> TokenAPI
    :<|> UserAPI
    :<|> UserEmailLinkAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI ServerContextM
applicationServer =
  infoServer
    :<|> apiKeyServer
    :<|> bootstrapServer
    :<|> documentTemplateServer
    :<|> knowledgeModelPackageServer
    :<|> localeServer
    :<|> persistentCommandServer
    :<|> tokenServer
    :<|> userServer
    :<|> userEmailLinkServer

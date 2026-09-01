module RegistryServer.Api.Handler.Api where

import Servant

import RegistryServer.Api.Handler.Config.Api
import RegistryServer.Api.Handler.DocumentTemplate.Api
import RegistryServer.Api.Handler.Info.Api
import RegistryServer.Api.Handler.KnowledgeModelPackage.Api
import RegistryServer.Api.Handler.Locale.Api
import RegistryServer.Api.Handler.Organization.Api
import RegistryServer.Api.Handler.PersistentCommand.Api
import RegistryServer.Api.Handler.UserEmailLink.Api
import RegistryServer.Model.Context.ServerContext

type ApplicationAPI =
  InfoAPI
    :<|> UserEmailLinkAPI
    :<|> ConfigAPI
    :<|> DocumentTemplateAPI
    :<|> KnowledgeModelPackageAPI
    :<|> LocaleAPI
    :<|> OrganizationAPI
    :<|> PersistentCommandAPI

applicationApi :: Proxy ApplicationAPI
applicationApi = Proxy

applicationServer :: ServerT ApplicationAPI ServerContextM
applicationServer =
  infoServer
    :<|> userEmailLinkServer
    :<|> configServer
    :<|> documentTemplateServer
    :<|> knowledgeModelPackageServer
    :<|> localeServer
    :<|> organizationServer
    :<|> persistentCommandServer

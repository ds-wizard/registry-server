module RegistryServer.Api.Handler.MailTemplateLocale.Api where

import Servant
import Servant.Swagger.Tags

import RegistryPublic.Api.Handler.MailTemplateLocale.Detail_Po_GET
import RegistryPublic.Api.Handler.MailTemplateLocale.List_GET
import RegistryServer.Api.Handler.MailTemplateLocale.Detail_Po_GET
import RegistryServer.Api.Handler.MailTemplateLocale.List_GET
import RegistryServer.Model.Context.ServerContext

type MailTemplateLocaleAPI =
  Tags "Mail Template Locale"
    :> ( List_GET
           :<|> Detail_Po_GET
       )

mailTemplateLocaleApi :: Proxy MailTemplateLocaleAPI
mailTemplateLocaleApi = Proxy

mailTemplateLocaleServer :: ServerT MailTemplateLocaleAPI ServerContextM
mailTemplateLocaleServer = list_GET :<|> detail_po_GET

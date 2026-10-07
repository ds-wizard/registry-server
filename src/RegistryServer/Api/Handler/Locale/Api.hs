module RegistryServer.Api.Handler.Locale.Api where

import Servant
import Servant.Swagger.Tags

import RegistryPublic.Api.Handler.Locale.Detail_Po_GET
import RegistryPublic.Api.Handler.Locale.List_GET
import RegistryServer.Api.Handler.Locale.Detail_Po_GET
import RegistryServer.Api.Handler.Locale.List_GET
import RegistryServer.Model.Context.ServerContext

type LocaleAPI =
  Tags "Locale"
    :> ( List_GET
           :<|> Detail_Po_GET
       )

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: ServerT LocaleAPI ServerContextM
localeServer = list_GET :<|> detail_po_GET

module RegistryServer.Api.Handler.Locale.Api where

import Servant

import RegistryPublic.Api.Handler.Locale.List_GET
import RegistryServer.Api.Handler.Locale.Detail_Bundle_GET
import RegistryServer.Api.Handler.Locale.Detail_GET
import RegistryServer.Api.Handler.Locale.List_Bundle_POST
import RegistryServer.Api.Handler.Locale.List_GET
import RegistryServer.Model.Context.ServerContext

type LocaleAPI =
  List_GET
    :<|> List_Bundle_POST
    :<|> Detail_GET
    :<|> Detail_Bundle_GET

localeApi :: Proxy LocaleAPI
localeApi = Proxy

localeServer :: ServerT LocaleAPI ServerContextM
localeServer = list_GET :<|> list_bundle_POST :<|> detail_GET :<|> detail_bundle_GET

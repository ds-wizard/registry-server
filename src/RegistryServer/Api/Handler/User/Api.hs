module RegistryServer.Api.Handler.User.Api where

import Servant
import Servant.Swagger.Tags

import RegistryServer.Api.Handler.User.Detail_DELETE
import RegistryServer.Api.Handler.User.Detail_GET
import RegistryServer.Api.Handler.User.Detail_Password_PUT
import RegistryServer.Api.Handler.User.Detail_State_PUT
import RegistryServer.Api.Handler.User.List_Current_GET
import RegistryServer.Api.Handler.User.List_Current_PUT
import RegistryServer.Api.Handler.User.List_Current_Password_PUT
import RegistryServer.Api.Handler.User.List_GET
import RegistryServer.Api.Handler.User.List_POST
import RegistryServer.Model.Context.ServerContext

type UserAPI =
  Tags "User"
    :> ( List_GET
           :<|> List_POST
           :<|> List_Current_GET
           :<|> List_Current_PUT
           :<|> List_Current_Password_PUT
           :<|> Detail_GET
           :<|> Detail_DELETE
           :<|> Detail_State_PUT
           :<|> Detail_Password_PUT
       )

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: ServerT UserAPI ServerContextM
userServer =
  list_GET
    :<|> list_POST
    :<|> list_current_GET
    :<|> list_current_PUT
    :<|> list_current_password_PUT
    :<|> detail_GET
    :<|> detail_DELETE
    :<|> detail_state_PUT
    :<|> detail_password_PUT

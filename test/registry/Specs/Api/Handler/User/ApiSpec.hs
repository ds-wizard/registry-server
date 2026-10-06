module Specs.Api.Handler.User.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai

import Specs.Api.Handler.Common
import Specs.Api.Handler.User.Detail_DELETE
import Specs.Api.Handler.User.Detail_GET
import Specs.Api.Handler.User.Detail_Password_PUT
import Specs.Api.Handler.User.Detail_State_PUT
import Specs.Api.Handler.User.List_Current_GET
import Specs.Api.Handler.User.List_Current_PUT
import Specs.Api.Handler.User.List_Current_Password_PUT
import Specs.Api.Handler.User.List_GET
import Specs.Api.Handler.User.List_POST

userAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "USER API Spec" $ do
      list_GET requestContext
      list_POST requestContext
      list_current_GET requestContext
      list_current_PUT requestContext
      list_current_password_PUT requestContext
      detail_GET requestContext
      detail_DELETE requestContext
      detail_state_PUT requestContext
      detail_password_PUT requestContext

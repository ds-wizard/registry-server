module Specs.Api.Handler.Organization.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai

import Specs.Api.Handler.Common
import Specs.Api.Handler.Organization.Detail_DELETE
import Specs.Api.Handler.Organization.Detail_GET
import Specs.Api.Handler.Organization.Detail_PUT
import Specs.Api.Handler.Organization.Detail_State_PUT
import Specs.Api.Handler.Organization.Detail_Token_PUT
import Specs.Api.Handler.Organization.List_GET
import Specs.Api.Handler.Organization.List_POST
import Specs.Api.Handler.Organization.List_Simple_GET

organizationAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "ORGANIZATION API Spec" $ do
      list_GET requestContext
      list_simple_GET requestContext
      list_POST requestContext
      detail_GET requestContext
      detail_PUT requestContext
      detail_DELETE requestContext
      detail_state_PUT requestContext
      detail_token_PUT requestContext

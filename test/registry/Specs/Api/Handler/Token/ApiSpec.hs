module Specs.Api.Handler.Token.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai

import Specs.Api.Handler.Common
import Specs.Api.Handler.Token.List_POST

tokenAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TOKEN API Spec" $
      list_POST requestContext

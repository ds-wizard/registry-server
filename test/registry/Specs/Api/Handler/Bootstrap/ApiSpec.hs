module Specs.Api.Handler.Bootstrap.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Bootstrap.List_GET
import Specs.Api.Handler.Common

bootstrapAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "BOOTSTRAP API Spec" $
      list_GET requestContext

module Specs.Api.Handler.DocumentTemplate.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.DocumentTemplate.Detail_Bundle_GET
import Specs.Api.Handler.DocumentTemplate.Detail_GET
import Specs.Api.Handler.DocumentTemplate.List_Bundle_POST
import Specs.Api.Handler.DocumentTemplate.List_GET

templateAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "TEMPLATE API Spec" $ do
      list_GET requestContext
      detail_GET requestContext
      detail_bundle_GET requestContext
      list_bundle_POST requestContext

module Specs.Api.Handler.Locale.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.Locale.Detail_Bundle_GET
import Specs.Api.Handler.Locale.Detail_GET
import Specs.Api.Handler.Locale.List_Bundle_POST
import Specs.Api.Handler.Locale.List_GET

localeAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $
    describe "LOCALE API Spec" $ do
      list_GET requestContext
      detail_GET requestContext
      detail_bundle_GET requestContext
      list_bundle_POST requestContext

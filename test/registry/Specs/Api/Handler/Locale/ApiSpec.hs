module Specs.Api.Handler.Locale.ApiSpec where

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.Locale.Detail_Po_GET
import Specs.Api.Handler.Locale.List_GET

localeAPI serverContext requestContext = do
  with (startWebApp serverContext requestContext) $
    describe "LOCALE API Spec" $ do
      list_GET requestContext
      detail_po_GET requestContext
  with (startWebApp (localeDisabledServerContext serverContext) requestContext) $
    describe "LOCALE API Spec (locales disabled)" $ do
      describe "GET /api/locales" $
        createFeatureDisabledTest methodGet "/api/locales" [] "" "Locale"
      describe "GET /api/locales/{code}/{version}/po" $
        createFeatureDisabledTest methodGet "/api/locales/de/5.0.0/po" [] "" "Locale"

module Specs.Api.Handler.MailTemplateLocale.ApiSpec where

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common

import Specs.Api.Handler.MailTemplateLocale.Detail_Po_GET
import Specs.Api.Handler.MailTemplateLocale.List_GET

mailTemplateLocaleAPI serverContext requestContext = do
  with (startWebApp serverContext requestContext) $
    describe "MAIL TEMPLATE LOCALE API Spec" $ do
      list_GET requestContext
      detail_po_GET requestContext
  with (startWebApp (localeDisabledServerContext serverContext) requestContext) $
    describe "MAIL TEMPLATE LOCALE API Spec (locales disabled)" $ do
      describe "GET /api/mail-template-locales" $
        createFeatureDisabledTest methodGet "/api/mail-template-locales" [] "" "Locale"
      describe "GET /api/mail-template-locales/{code}/{version}/po" $
        createFeatureDisabledTest methodGet "/api/mail-template-locales/de/5.0.0/po" [] "" "Locale"

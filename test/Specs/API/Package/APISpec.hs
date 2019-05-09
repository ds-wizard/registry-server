module Specs.API.Package.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.Package.Detail_Export_GET
import Specs.API.Package.Detail_GET
import Specs.API.Package.List_Unique_GET

packageAPI appContext =
  with (startWebApp appContext) $
  describe "PACKAGE API Spec" $ do
    list_unique_get appContext
    detail_get appContext
    detail_export_get appContext

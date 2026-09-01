module Specs.Api.Handler.UserEmailLink.ApiSpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.Api.Handler.Common
import Specs.Api.Handler.UserEmailLink.List_POST

userEmailLinkAPI serverContext requestContext =
  with (startWebApp serverContext requestContext) $ describe "ACTION KEY API Spec" $ list_POST requestContext

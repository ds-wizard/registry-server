module Specs.Api.Handler.Audit.Common where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import RegistryServer.Database.DAO.Audit.AuditEntryDAO

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfAuditEntryInDB requestContext ae = do
  aeD <- getFirstFromDB findAuditEntries requestContext
  liftIO $ ae `shouldBe` aeD

module Specs.Api.Handler.Organization.Common where

import Test.Hspec
import Test.Hspec.Wai

import RegistryPublic.Model.Organization.Organization
import RegistryServer.Database.DAO.Organization.OrganizationDAO

import Specs.Api.Handler.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfOrganizationInDB requestContext organization = do
  organizationFromDb <- getOneFromDB (findOrganizationByOrgId organization.organizationId) requestContext
  compareOrganizationDtos organizationFromDb organization

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareOrganizationDtosWhenCreate resDto expDto = do
  liftIO $ resDto.organizationId `shouldBe` expDto.organizationId
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.description `shouldBe` expDto.description
  liftIO $ resDto.email `shouldBe` expDto.email

compareOrganizationDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

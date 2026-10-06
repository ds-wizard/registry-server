module Specs.Api.Handler.User.Common where

import Test.Hspec
import Test.Hspec.Wai

import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Model.User.User

import Specs.Api.Handler.Common

assertExistenceOfUserInDB requestContext user = do
  userFromDb <- getOneFromDB (findUserByUuid user.uuid) requestContext
  liftIO $ userFromDb `shouldBe` user

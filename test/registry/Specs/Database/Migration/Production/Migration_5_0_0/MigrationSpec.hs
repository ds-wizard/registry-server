module Specs.Database.Migration.Production.Migration_5_0_0.MigrationSpec where

import Control.Monad.Logger (runLoggingT)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple
import Test.Hspec

import RegistryServer.Database.DAO.User.UserDAO
import RegistryServer.Database.Migration.Production.Migration_5_0_0.Migration
import RegistryServer.Model.Context.RequestContext
import RegistryServer.Model.User.User
import Shared.Util.Crypto (hashSHA256)
import Shared.Util.Password

import Specs.Common

migration_5_0_0Spec requestContext =
  describe "Migration 5.0.0" $ do
    it "turns an organization into an account with a migrated API key" $ do
      -- GIVEN: Prepare DB
      createOrganizations requestContext [("org.de", "Germany", "germany@example.com", "GermanyToken")]
      -- WHEN: Run migration
      runLoggingT (migrateOrganizations requestContext.dbPool) (\_ _ _ _ -> return ())
      -- THEN: Check the account
      Right user <- runInContext (findUserByEmail "germany@example.com") requestContext
      user.firstName `shouldBe` "germany@example.com"
      user.lastName `shouldBe` ""
      user.role `shouldBe` UserRole
      user.active `shouldBe` True
      verifyPassword "GermanyToken" user.passwordHash `shouldBe` True
      -- AND: Check the API key
      Right mTokenUser <- runInContext (findUserByTokenHash' (hashSHA256 "GermanyToken")) requestContext
      fmap (.uuid) mTokenUser `shouldBe` Just user.uuid
      tokens <- withResource requestContext.dbPool $ \conn ->
        query conn "SELECT name, type, expires_at IS NULL FROM user_token WHERE user_uuid = ?" (Only user.uuid)
      tokens `shouldBe` [("Migrated token", "ApiKeyUserTokenType", True) :: (String, String, Bool)]
      -- CLEAN UP
      dropOrganizations requestContext
    it "refuses duplicate organization emails" $ do
      -- GIVEN: Prepare DB
      createOrganizations requestContext [("org.de", "Germany", "germany@example.com", "GermanyToken"), ("org.at", "Austria", "germany@example.com", "AustriaToken")]
      -- WHEN: Run the assert
      runLoggingT (assertNoDuplicateEmails requestContext.dbPool) (\_ _ _ _ -> return ())
        `shouldThrow` (\e -> "germany@example.com" `BS.isInfixOf` sqlErrorMsg e)
      -- CLEAN UP
      dropOrganizations requestContext

createOrganizations requestContext organizations =
  withResource requestContext.dbPool $ \conn -> do
    _ <- execute_ conn "DROP TABLE IF EXISTS organization CASCADE"
    _ <- execute_ conn "CREATE TABLE organization (organization_id varchar, name varchar, description varchar, email varchar, role varchar, token varchar, active boolean, logo varchar, created_at timestamptz, updated_at timestamptz)"
    traverse_ (execute conn "INSERT INTO organization VALUES (?, ?, '', ?, 'UserRole', ?, true, NULL, now(), now())") (organizations :: [(String, String, String, String)])

dropOrganizations requestContext = do
  _ <- withResource requestContext.dbPool $ \conn -> execute_ conn "DROP TABLE organization"
  return ()

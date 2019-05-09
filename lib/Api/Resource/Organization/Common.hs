module Api.Resource.Organization.Common where

import Model.Organization.Organization

serializeOrganizationRole :: OrganizationRole -> String
serializeOrganizationRole orgRole =
  case orgRole of
    AdminRole -> "AdminRole"
    UserRole -> "UserRole"

deserializeOrganizationRole :: String -> Maybe OrganizationRole
deserializeOrganizationRole "AdminRole" = Just AdminRole
deserializeOrganizationRole "UserRole" = Just UserRole
deserializeOrganizationRole _ = Nothing

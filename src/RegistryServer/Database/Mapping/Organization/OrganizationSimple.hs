module RegistryServer.Database.Mapping.Organization.OrganizationSimple where

import Database.PostgreSQL.Simple

import RegistryPublic.Model.Organization.OrganizationSimple

instance FromRow OrganizationSimple

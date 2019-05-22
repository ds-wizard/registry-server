module Database.BSON.Organization.Organization where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Model.Organization.Organization

instance ToBSON Organization where
  toBSON Organization {..} =
    [ "organizationId" BSON.=: _organizationOrganizationId
    , "name" BSON.=: _organizationName
    , "description" BSON.=: _organizationDescription
    , "email" BSON.=: _organizationEmail
    , "role" BSON.=: serializeOrganizationRole _organizationRole
    , "token" BSON.=: _organizationToken
    , "active" BSON.=: _organizationActive
    , "logo" BSON.=: _organizationLogo
    , "createdAt" BSON.=: _organizationCreatedAt
    , "updatedAt" BSON.=: _organizationUpdatedAt
    , "lastAccessAt" BSON.=: _organizationLastAccessAt
    ]

instance FromBSON Organization where
  fromBSON doc = do
    _organizationOrganizationId <- BSON.lookup "organizationId" doc
    _organizationName <- BSON.lookup "name" doc
    _organizationDescription <- BSON.lookup "description" doc
    _organizationEmail <- BSON.lookup "email" doc
    _organizationRole <- deserializeOrganizationRole $ BSON.lookup "role" doc
    _organizationToken <- BSON.lookup "token" doc
    _organizationActive <- BSON.lookup "active" doc
    _organizationLogo <- BSON.lookup "logo" doc
    _organizationCreatedAt <- BSON.lookup "createdAt" doc
    _organizationUpdatedAt <- BSON.lookup "updatedAt" doc
    _organizationLastAccessAt <- BSON.lookup "lastAccessAt" doc
    return Organization {..}

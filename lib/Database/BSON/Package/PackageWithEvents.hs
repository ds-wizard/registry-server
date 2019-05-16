module Database.BSON.Package.PackageWithEvents where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Event.Answer ()
import Database.BSON.Event.Chapter ()
import Database.BSON.Event.Common
import Database.BSON.Event.Expert ()
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.Event.Question ()
import Database.BSON.Event.Reference ()
import LensesConfig
import Model.Package.Package

instance ToBSON PackageWithEvents where
  toBSON package =
    [ "id" BSON.=: package ^. pId
    , "name" BSON.=: (package ^. name)
    , "organizationId" BSON.=: (package ^. organizationId)
    , "kmId" BSON.=: (package ^. kmId)
    , "version" BSON.=: (package ^. version)
    , "metamodelVersion" BSON.=: (package ^. metamodelVersion)
    , "description" BSON.=: (package ^. description)
    , "parentPackageId" BSON.=: (package ^. parentPackageId)
    , "events" BSON.=: convertEventToBSON <$> (package ^. events)
    ]

instance FromBSON PackageWithEvents where
  fromBSON doc = do
    pkgPId <- BSON.lookup "id" doc
    pkgName <- BSON.lookup "name" doc
    pkgOrganizationId <- BSON.lookup "organizationId" doc
    pkgKmId <- BSON.lookup "kmId" doc
    pkgVersion <- BSON.lookup "version" doc
    pkgMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    pkgDescription <- BSON.lookup "description" doc
    pkgParentPackageId <- BSON.lookup "parentPackageId" doc
    pkgEventsSerialized <- BSON.lookup "events" doc
    let pkgEvents = (fromJust . chooseEventDeserializator) <$> pkgEventsSerialized
    return
      PackageWithEvents
      { _packageWithEventsPId = pkgPId
      , _packageWithEventsName = pkgName
      , _packageWithEventsOrganizationId = pkgOrganizationId
      , _packageWithEventsKmId = pkgKmId
      , _packageWithEventsVersion = pkgVersion
      , _packageWithEventsMetamodelVersion = pkgMetamodelVersion
      , _packageWithEventsDescription = pkgDescription
      , _packageWithEventsParentPackageId = pkgParentPackageId
      , _packageWithEventsEvents = pkgEvents
      }

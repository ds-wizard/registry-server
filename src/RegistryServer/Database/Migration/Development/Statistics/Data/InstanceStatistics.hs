module RegistryServer.Database.Migration.Development.Statistics.Data.InstanceStatistics where

import RegistryServer.Model.Statistics.InstanceStatistics

iStat :: InstanceStatistics
iStat =
  InstanceStatistics
    { userCount = 10
    , pkgCount = 20
    , prjCount = 30
    , kmEditorCount = 40
    , docCount = 50
    , tmlCount = 60
    }

module RegistryServer.Constant.UserToken where

import Data.Time

loginTokenExpiration :: NominalDiffTime
loginTokenExpiration = 14 * nominalDay

module RegistryServer.Service.Locale.LocaleUtil where

import Shared.Model.Locale.Locale
import Shared.Util.List (groupBy)

groupLocales :: [Locale] -> [[Locale]]
groupLocales = groupBy (\t1 t2 -> t1.id == t2.id)

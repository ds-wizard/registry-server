module Database.Migration.Production.Migration_0004_metrics_init.Data.Metrics where

import qualified Data.Bson as BSON

metricF now =
  [ "uuid" BSON.=: "8db30660-d4e5-4c0a-bf3e-553f3f0f997a"
  , "title" BSON.=: "Findability"
  , "abbreviation" BSON.=: "F"
  , "description" BSON.=:
    "The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricA now =
  [ "uuid" BSON.=: "0feac7e6-add4-4723-abae-be5ce7864c63"
  , "title" BSON.=: "Accessibility"
  , "abbreviation" BSON.=: "A"
  , "description" BSON.=:
    "The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!"
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricI now =
  [ "uuid" BSON.=: "a42bded3-a085-45f8-b384-32b4a77c8385"
  , "title" BSON.=: "Interoperability"
  , "abbreviation" BSON.=: "I"
  , "description" BSON.=:
    "The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and 'understand' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricR now =
  [ "uuid" BSON.=: "0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7"
  , "title" BSON.=: "Reusability"
  , "abbreviation" BSON.=: "R"
  , "description" BSON.=:
    "The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent)."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricG now =
  [ "uuid" BSON.=: "8845fe2b-79df-4138-baea-3a035bf5e249"
  , "title" BSON.=: "Good DMP Practice"
  , "abbreviation" BSON.=: "G"
  , "description" BSON.=:
    "The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

metricO now =
  [ "uuid" BSON.=: "cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0"
  , "title" BSON.=: "Openness"
  , "abbreviation" BSON.=: "O"
  , "description" BSON.=:
    "The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use."
  , "references" BSON.=: ([] :: [String])
  , "createdAt" BSON.=: now
  , "updatedAt" BSON.=: now
  ]

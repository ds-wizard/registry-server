module Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import Constant.KnowledgeModel
import Database.Migration.Development.Event.Data.Events
import LensesConfig
import Model.Event.Event
import Model.Package.PackageWithEvents
import Service.Package.PackageMapper

globalPackageEmpty :: PackageWithEvents
globalPackageEmpty =
  buildPackage
    "DSW Global Knowledge Model"
    "dsw"
    "core"
    "0.0.1"
    kmMetamodelVersion
    "Empty package"
    "# DSW Global Knowledge Model"
    Nothing
    []
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

globalPackage :: PackageWithEvents
globalPackage =
  buildPackage
    "DSW Global Knowledge Model"
    "dsw"
    "core"
    "1.0.0"
    kmMetamodelVersion
    "First Release"
    "# DSW Global Knowledge Model"
    Nothing
    [ AddKnowledgeModelEvent' a_km1
    , AddTagEvent' a_km1_tds
    , AddTagEvent' a_km1_tbi
    , AddIntegrationEvent' a_km1_iop
    , AddIntegrationEvent' a_km1_ibp
    ]
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

netherlandsPackage :: PackageWithEvents
netherlandsPackage =
  buildPackage
    "DSW Netherlands Knowledge Model"
    "dsw.nl"
    "core-nl"
    "1.0.0"
    kmMetamodelVersion
    "First Release"
    "# DSW Netherlands Knowledge Model"
    (Just $ globalPackage ^. pId)
    [AddChapterEvent' a_km1_ch1]
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

netherlandsPackageV2 :: PackageWithEvents
netherlandsPackageV2 =
  buildPackage
    "DSW Netherlands Knowledge Model"
    "dsw.nl"
    "core-nl"
    "2.0.0"
    kmMetamodelVersion
    "Second Release"
    "# DSW Netherlands Knowledge Model"
    (Just $ netherlandsPackage ^. pId)
    [AddChapterEvent' a_km1_ch4]
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

amsterdamPackage :: PackageWithEvents
amsterdamPackage =
  buildPackage
    "DSW Amsterdam Knowledge Model"
    "dsw.nl.amsterdam"
    "core-amsterdam"
    "1.0.0"
    kmMetamodelVersion
    "First Release"
    "# DSW Amsterdam Knowledge Model"
    (Just $ netherlandsPackage ^. pId)
    []
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

germanyPackage :: PackageWithEvents
germanyPackage =
  buildPackage
    "DSW Germany"
    "dsw.de"
    "core-de"
    "1.0.0"
    kmMetamodelVersion
    "First Release"
    "# DSW Germany"
    (Just $ globalPackageEmpty ^. pId)
    [ AddKnowledgeModelEvent' a_km1
    , AddTagEvent' a_km1_tds
    , AddTagEvent' a_km1_tbi
    , AddIntegrationEvent' a_km1_iop
    , AddIntegrationEvent' a_km1_ibp
    , AddChapterEvent' a_km1_ch1
    , AddQuestionEvent' a_km1_ch1_q1'
    , AddQuestionEvent' a_km1_ch1_q2'
    , AddAnswerEvent' a_km1_ch1_q2_aNo1
    , AddAnswerEvent' a_km1_ch1_q2_aYes1
    , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1'
    , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
    , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
    , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2'
    , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
    , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
    , AddExpertEvent' a_km1_ch1_q2_eAlbert
    , AddExpertEvent' a_km1_ch1_q2_eNikola
    , AddReferenceEvent' a_km1_ch1_q2_rCh1'
    , AddReferenceEvent' a_km1_ch1_q2_rCh2'
    , AddChapterEvent' a_km1_ch2
    , AddQuestionEvent' a_km1_ch2_q3'
    , AddAnswerEvent' a_km1_ch2_q3_aNo2
    , AddAnswerEvent' a_km1_ch2_q3_aYes2
    , AddQuestionEvent' a_km1_ch2_q4'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q5'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q7'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q8'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q6'
    , AddAnswerEvent' a_km1_ch2_q4_it_q6_aNo
    , AddAnswerEvent' a_km1_ch2_q4_it_q6_aYes
    , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4'
    , AddQuestionEvent' a_km1_ch2_ansYes6_fuq5'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1'
    , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2'
    , AddExpertEvent' a_km1_ch2_q6_eAlbert
    , AddExpertEvent' a_km1_ch2_q6_eNikola
    , AddReferenceEvent' a_km1_ch2_q6_rCh1'
    , AddReferenceEvent' a_km1_ch2_q6_rCh2'
    , AddChapterEvent' a_km1_ch3
    , AddQuestionEvent' a_km1_ch3_q9'
    , AddQuestionEvent' a_km1_ch3_q10'
    ]
    (UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0)

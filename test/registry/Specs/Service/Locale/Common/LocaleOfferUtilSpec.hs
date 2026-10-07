module Specs.Service.Locale.Common.LocaleOfferUtilSpec where

import Test.Hspec

import RegistryServer.Service.Locale.Common.LocaleOfferUtil

localeOfferUtilSpec =
  describe "LocaleOfferUtil" $ do
    it "parseVersion" $ do
      parseVersion "5.0.3" `shouldBe` Just [5, 0, 3]
      parseVersion "dsw-client:4.10.0" `shouldBe` Just [4, 10, 0]
      parseVersion "next" `shouldBe` Nothing
      parseVersion "" `shouldBe` Nothing
    it "selectOffered" $ do
      let offer mAppVersion = selectOffered mAppVersion fst snd versions
      offer Nothing `shouldBe` [("cs", "6.0.0"), ("de", "5.1.0"), ("nl", "4.10.0")]
      offer (Just "5.0.3") `shouldBe` [("cs", "5.2.0"), ("de", "5.0.0"), ("nl", "4.10.0")]
      offer (Just "5.1.0") `shouldBe` [("cs", "5.2.0"), ("de", "5.1.0"), ("nl", "4.10.0")]
      offer (Just "1.0.0") `shouldBe` [("cs", "5.2.0"), ("de", "4.0.0"), ("nl", "4.9.0")]
      selectOffered (Just "5.0.0") fst snd [("es", "next")] `shouldBe` [("es", "next")]

versions :: [(String, String)]
versions =
  [ ("de", "5.1.0")
  , ("nl", "next")
  , ("de", "4.0.0")
  , ("cs", "6.0.0")
  , ("nl", "4.9.0")
  , ("de", "5.0.0")
  , ("cs", "5.2.0")
  , ("nl", "4.10.0")
  ]

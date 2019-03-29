{-# LANGUAGE TypeApplications #-}
module Network.AWS.SNS.Notification.SESSpec (main, spec) where

import           Control.Monad                    (forM_)
import           Data.Aeson                       (eitherDecodeFileStrict)
import           Data.Either                      (isRight)
import           Network.AWS.SNS.Notification.SES (Notification)
import           System.FilePath.Glob             (compile, globDir1)
import           Test.Hspec                       (Spec, describe, hspec, it,
                                                   runIO, shouldSatisfy)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Decode real responses" $ do
    paths <- runIO $ globDir1 (compile "*.json") "spec/fixtures"

    forM_ paths $ \path ->
      it ("decodes " <> path) $ do
        eDecoded <- eitherDecodeFileStrict @Notification path
        eDecoded `shouldSatisfy` isRight

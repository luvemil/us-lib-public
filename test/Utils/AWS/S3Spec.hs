module Utils.AWS.S3Spec where

import Control.Lens ()
import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Hspec
import Utils.AWS.Loaders
import Utils.AWS.S3

main :: IO ()
main = hspec spec

spec :: Spec
{-# NOINLINE spec #-}
spec = do
    runTest :: Bool <-
        runIO $
            readIO @Bool . fromMaybe "False"
                =<< lookupEnv "TEST_AWS_INTEGRATION"
    when runTest $ do
        describe "getFile" $ do
            it "gets the file" $ do
                res :: BS.ByteString <- do
                    region <- fromEnv "TEST_AWS_REGION"
                    bucket <- fromEnv "TEST_AWS_BUCKET"
                    objKey <- fromEnv "TEST_AWS_S3_PATH"
                    getFile region bucket objKey
                res `shouldNotBe` ""
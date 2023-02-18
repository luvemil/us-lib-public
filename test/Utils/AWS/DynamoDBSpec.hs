module Utils.AWS.DynamoDBSpec where

import Control.Lens
import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Hspec
import Utils.AWS.DynamoDB
import Utils.AWS.Loaders
import Utils.AWS.PutAeson

main :: IO ()
main = hspec spec

testData :: [Aeson.Object]
testData =
    [ KM.fromList [("shortcode", Aeson.String "test-001")]
    , KM.fromList
        [ ("shortcode", Aeson.String "test-002")
        ,
            ( "subObj"
            , Aeson.Object
                ( KM.fromList [("key1", Aeson.Number 123), ("key2", Aeson.String "value")]
                )
            )
        ]
    ]

spec :: Spec
{-# NOINLINE spec #-}
spec = do
    runTest :: Bool <-
        runIO $
            readIO @Bool . fromMaybe "False"
                =<< lookupEnv "TEST_AWS_INTEGRATION"
    when runTest $ do
        describe "saveToDynamoDB" $ do
            it "saves the data" $ do
                _ <- do
                    region <- fromEnv "TEST_AWS_REGION"
                    tableName <- fromEnv "TEST_AWS_TABLE_NAME"
                    let putItems = map (buildPutPostObject tableName) testData
                    saveToDynamoDB region putItems
                True `shouldBe` True
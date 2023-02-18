module Utils.AWS.Loaders where

import Amazonka
import Data.Text qualified as T
import System.Environment (getEnv)

fromString :: FromText a => String -> Either String a
fromString = fromText . T.pack

fromEnv :: FromText a => String -> IO a
fromEnv s = embedEither . fromString =<< getEnv s
  where
    embedEither (Left e) = fail e
    embedEither (Right x) = pure x
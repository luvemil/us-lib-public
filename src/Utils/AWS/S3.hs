module Utils.AWS.S3 where

import Amazonka hiding (length)
import Amazonka.S3
import Conduit
import Control.Lens
import Data.ByteString qualified as BS
import Data.Generics.Labels ()
import System.IO

getFile ::
    -- | Region to operate in.
    Region ->
    BucketName ->
    -- | The source object key.
    ObjectKey ->
    IO BS.ByteString
getFile reg b k = do
    -- lgr <- newLogger Debug stdout
    -- env <- newEnv discover <&> set #logger lgr . set #region reg

    env <- newEnv discover <&> set #region reg

    runResourceT $ do
        rs <- send env (newGetObject b k)
        r <- view #body rs `sinkBody` sinkLazy
        pure $ BS.toStrict r

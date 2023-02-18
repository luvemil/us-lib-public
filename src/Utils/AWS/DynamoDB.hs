module Utils.AWS.DynamoDB where

import Amazonka
import Amazonka.DynamoDB
import Control.Lens
import Data.Generics.Labels ()

saveToDynamoDB :: Region -> [PutItem] -> IO ()
saveToDynamoDB region items = do
    env <- newEnv discover <&> set #region region
    runResourceT $ do
        mapM_ (send env) items

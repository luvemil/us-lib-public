module Utils.AWS.PutAeson where

import Amazonka.DynamoDB
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Except
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as M
import Data.Text qualified as T
import Utils.Internal

toAttributeValue :: Aeson.Value -> AttributeValue
toAttributeValue Aeson.Null = NULL
toAttributeValue (Aeson.Bool v) = BOOL v
toAttributeValue (Aeson.Number v) = N (T.pack . show $ v)
toAttributeValue (Aeson.String v) = S v
toAttributeValue (Aeson.Array vs) = L (over traversed toAttributeValue vs)
toAttributeValue (Aeson.Object hm) = M (hm & toAttributeValueHM & M.fromList) -- (Aeson.toList hm & traversed %~ (\j -> toAttributeValue j))

toAttributeValueHM :: Aeson.Object -> [(T.Text, AttributeValue)]
toAttributeValueHM obj =
    let listed = Aeson.toList obj
        attrs :: [(T.Text, AttributeValue)] =
            map
                (\(k, v) -> (Aeson.toText k, toAttributeValue v))
                listed
     in attrs

mapToAV :: Aeson.Object -> HM.HashMap T.Text AttributeValue -- [(T.Text, AttributeValue)]
mapToAV obj = HM.fromList $ toAttributeValueHM obj

buildPutPostObject :: T.Text -> Aeson.Object -> PutItem
buildPutPostObject table postO =
    newPutItem table & #item .~ mapToAV postO

buildPutItemsObjectList :: Monad m => Aeson.ToJSON v => T.Text -> v -> ExceptT String m [PutItem]
buildPutItemsObjectList tableName v = do
    let val = Aeson.toJSON v
    items <- case val of
        Aeson.Object obj -> pure [obj]
        Aeson.Array vec -> do
            pure $ (vec ^.. traversed) & evalPrism id _Object
        _ -> throwE "Value is not a json Object"
    pure $ map (buildPutPostObject tableName) items

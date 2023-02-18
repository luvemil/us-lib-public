{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Crawl.Feed.Effect where

import Polysemy

data Feed res sum det m a where
    GetItems :: Maybe res -> Feed res sum det m (Maybe res)
    GetDetail :: sum -> Feed res sum det m det

makeSem ''Feed

runFeedDetailMap :: forall res sum det det' r a. (det' -> det) -> Sem (Feed res sum det ': r) a -> Sem (Feed res sum det' ': r) a
runFeedDetailMap fn = reinterpret $ \case
    GetItems r -> getItems @res @sum @det' r
    GetDetail s -> fn <$> getDetail @res @sum @det' s

{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
module Crawl.Media.Feed where

import Crawl.Feed
import Crawl.Media.Effect
import Crawl.Media.Types
import Polysemy
import Polysemy.Input
import Polysemy.Error
import Control.Lens

toCdnMedia :: Members '[ MediaStore, Error String ] r => MediaDesc -> Sem r MediaDesc
toCdnMedia (RemoteMedia (MediaUrl url)) = do
    res <- downloadMedia url
    case res of
        Left s -> throw s
        Right c -> pure $ CDNMedia c
toCdnMedia x@(CDNMedia _) = pure x
toCdnMedia _ = throw @String "Not found"

populateMedia :: Members '[ MediaStore, Error String ] r => Traversal' s MediaDesc -> s -> Sem r s
populateMedia theFold s =
    s & theFold %%~ \md -> catch @String (toCdnMedia md) $ \_ -> pure md

{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}
module Crawl.Feed.Actions where

import Crawl.Feed.Effect
import Crawl.Feed.Types
import Control.Lens
import Polysemy
import Polysemy.Input

recursiveGetItems :: 
    forall res sum det r. Members '[Feed res sum det, Input (FeedConfig res sum det)] r
    => [res]
    -> Maybe res
    -> Sem r [res]
recursiveGetItems acc Nothing = pure acc
recursiveGetItems acc (Just res) = do
    nextRes <- getItems @res @sum @det (Just res)
    let nextAcc = acc <> [res]
    recursiveGetItems @res @sum @det nextAcc nextRes

downloadAllResponses :: forall res sum det r. Members '[Feed res sum det, Input (FeedConfig res sum det)] r => Sem r [res]
downloadAllResponses = do
    fstRes <- getItems @res @sum @det Nothing
    recursiveGetItems @res @sum @det [] fstRes

downloadFullProfile :: forall res sum det r. Members '[Feed res sum det, Input (FeedConfig res sum det)] r => Sem r [det]
downloadFullProfile = do
    FeedConfig {..} <- input @(FeedConfig res sum det)
    allRes <- downloadAllResponses @res @sum @det
    let allSum = allRes ^.. traversed . fcPosts
    allSum & traversed %%~ getDetail @res @sum @det
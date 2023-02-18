module Crawl.Feed.Types where

import Control.Lens

import Crawl.Media.Types

data FeedConfig res sum det = FeedConfig
    { fcPosts :: Fold res sum
    , fcMedia :: Traversal' det MediaDesc
    }

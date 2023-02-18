{-# LANGUAGE TemplateHaskell #-}
module Crawl.Media.Types where

import Control.Lens

newtype CDNRef = CDNRef {unCRDRef :: String}
    deriving newtype (Eq, Show)

newtype MediaUrl = MediaUrl {unMediaUrl :: String}
    deriving newtype (Eq, Show)

data MediaDesc
    = RemoteMedia MediaUrl -- use a better type to wrap an url
    | CDNMedia CDNRef -- instead of String we should have a describing a CDN, e.g. S3
    | MediaNotFound
    deriving (Show, Eq)

makePrisms ''MediaDesc

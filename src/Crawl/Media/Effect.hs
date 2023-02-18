{-# LANGUAGE TemplateHaskell #-}
module Crawl.Media.Effect where

import Polysemy
import Crawl.Media.Types

data MediaStore m a where
    DownloadMedia :: String -> MediaStore m (Either String CDNRef)
    GetMediaUrl :: CDNRef -> MediaStore m (Either String MediaUrl)

makeSem ''MediaStore

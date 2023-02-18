module Crawl.Media.Actions where

import Crawl.Media.Effect
import Crawl.Media.Types
import Polysemy

saveMediaToCDN :: Members '[MediaStore] r => String -> Sem r MediaDesc
saveMediaToCDN mediaUrl = do
    res <- downloadMedia mediaUrl
    case res of
        Left _ -> pure MediaNotFound
        Right s -> pure $ CDNMedia s

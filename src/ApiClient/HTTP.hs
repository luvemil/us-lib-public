module ApiClient.HTTP where

import ApiClient.Types
import Control.Lens
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Foldable qualified as F
import Network.Wreq qualified as Wreq
import Network.Wreq.Lens
import System.FilePath

setHeaders :: ApiConfig -> Wreq.Options
setHeaders ac =
    let hs = ac ^. #acHeaders
     in F.foldl'
            (\acc (hName, hVal) -> acc & header hName .~ hVal)
            Wreq.defaults
            hs

httpGet :: String -> IO String
httpGet endpoint = do
    res <- Wreq.get endpoint
    let b = res ^. responseBody
    pure $ BSL.unpack b

httpGetWith :: ApiConfig -> String -> IO String
httpGetWith ac path = do
    let options =
            setHeaders ac
                -- Don'r redirect
                & redirects .~ 0
                -- Keep all responses
                & checkResponse ?~ \_ _ -> pure ()
        target = ac ^. #acBaseUrl </> path
    res <- Wreq.getWith options target
    let b = res ^. responseBody
    pure $ BSL.unpack b

httpGetFile :: Wreq.Options -> String -> FilePath -> IO ()
httpGetFile opts url destPath = do
    let opts' = opts & header "accept" .~ ["application/octet-stream"]
    res <- Wreq.getWith opts' url
    let bResM = res ^? responseBody
    case bResM of
        Just bRes -> BSL.writeFile destPath bRes
        Nothing -> fail "Empty content"

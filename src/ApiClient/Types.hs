module ApiClient.Types where

import Control.Lens ()
import Data.ByteString
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.Text
import GHC.Generics
import Network.HTTP.Types.Header

type HeaderConfig = (HeaderName, [ByteString])

data ApiConfig = ApiConfig
    { acBaseUrl :: String
    , acHeaders :: [HeaderConfig]
    }
    deriving (Show, Eq, Generic)

newtype ApiError = ApiError {unText :: Text}
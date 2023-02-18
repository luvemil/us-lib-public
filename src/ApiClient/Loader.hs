module ApiClient.Loader where

import ApiClient.Types
import Data.ByteString.Char8 qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Toml (TomlCodec, (.=))
import Toml qualified

apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec =
    ApiConfig
        <$> (Toml.string "url" .= acBaseUrl)
        <*> (headersCodec .= acHeaders)

headersCodec :: TomlCodec [HeaderConfig]
headersCodec = Toml.list midCodec "headers"

midCodec :: TomlCodec HeaderConfig
-- midCodec = _f fromHCO toHCO headerCodec
midCodec = fromHCO <$> (headerCodec .= toHCO)

data HeaderConfigO = HeaderConfigO
    { hcoName :: String
    , hcoValue :: String
    }

fromHCO :: HeaderConfigO -> HeaderConfig
fromHCO (HeaderConfigO name value) = (CI.mk (BS.pack name), [BS.pack value])

toHCO :: HeaderConfig -> HeaderConfigO
toHCO _ = undefined

headerCodec :: TomlCodec HeaderConfigO
headerCodec =
    HeaderConfigO
        <$> (Toml.string "name" .= hcoName)
        <*> (Toml.string "value" .= hcoValue)

loadConfig :: FilePath -> IO ApiConfig
loadConfig = Toml.decodeFile apiConfigCodec

loadConfigText :: T.Text -> Either String ApiConfig
loadConfigText s =
    case Toml.decode apiConfigCodec s of
        Right x -> Right x
        Left e -> Left . show $ Toml.prettyTomlDecodeErrors e

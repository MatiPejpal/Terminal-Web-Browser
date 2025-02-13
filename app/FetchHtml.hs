module FetchHtml where

-- Network
import qualified Network.HTTP.Simple as S
import Network.HTTP.Client.TLS 
import Network.HTTP.Client

-- Text encoding
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS
import TUI 
import HtmlTree (createTree)

-- Fetch HTML content
fetch :: String -> IO TuiState
fetch url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    let htmlContent = TE.decodeUtf8 $ LBS.toStrict $ S.getResponseBody response
    let tree = createTree $ show htmlContent
    let its = initialTuiState
    return $ its { html = Just tree }


module FetchHtml where

-- Network
import qualified Network.HTTP.Simple as S
import Network.HTTP.Client.TLS 
import Network.HTTP.Client

-- Text encoding
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS

-- Fetch HTML content
fetch :: String -> IO String
fetch url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    let html = TE.decodeUtf8 $ LBS.toStrict $ S.getResponseBody response
    return $ show html


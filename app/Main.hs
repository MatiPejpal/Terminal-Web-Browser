module Main where

{-# LANGUAGE OverloadedStrings #-}

-- Network
import qualified Network.HTTP.Simple as S
import Network.HTTP.Client.TLS 
import Network.HTTP.Client

-- Text encoding
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS

-- Project modules
import HtmlTree
import TUI

-- Other imports
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    -- Prompt user for URL
    putStr "Enter website URL (include http:// or https://): "
    hFlush stdout
    url <- getLine

    -- Fetch HTML content
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    let html = TE.decodeUtf8 $ LBS.toStrict $ S.getResponseBody response
    
    -- Print Tree
    putStrLn "=== Tree ==="
    printHtml $ createTree $ show html

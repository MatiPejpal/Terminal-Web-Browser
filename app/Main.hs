module Main where
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Simple as S
import Network.HTTP.Client.TLS 
import Network.HTTP.Client
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO (hFlush, stdout)
import HtmlTree

-- Function that prints html tree
indent :: Int -> String
indent n = replicate (n * 2) ' '

printAttr :: TagAttr -> String
printAttr (TagAttr name val) = name ++ "=\"" ++ val ++ "\""

printAttrs :: [TagAttr] -> String
printAttrs attrs = if null attrs then "" else " " ++ unwords (map printAttr attrs)

printHtmlTree :: Int -> HtmlTree -> String
printHtmlTree level (Text content) = indent level ++ "Text: " ++ content ++ "\n"
printHtmlTree level (HtmlTag tagType attrs children) =
    let openingTag = tagType ++ printAttrs attrs
        childrenStr = concatMap (printHtmlTree (level + 1)) children
    in indent level ++ openingTag ++ "\n" ++ childrenStr ++ indent level ++ "\n"

printHtml :: HtmlTree -> IO ()
printHtml tree = putStr (printHtmlTree 0 tree)

main :: IO ()
main = do
    putStr "Enter website URL (include http:// or https://): "
    hFlush stdout
    url <- getLine

    -- Fetch HTML content
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    let html = L8.unpack $ S.getResponseBody response
    putStrLn "=== Tree ==="
    printHtml $ createTree html

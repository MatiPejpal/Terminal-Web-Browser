module Main where
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Simple as S
import Network.HTTP.Client.TLS 
import Network.HTTP.Client
import System.IO (hFlush, stdout)
import HtmlTree
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS
import Data.Char (chr, isDigit)
import Numeric (readDec)

-- Function that prints html tree
indent :: Int -> String
indent n = replicate (n * 2) ' '

printAttr :: TagAttr -> String
printAttr (TagAttr name val) = name ++ "=\"" ++ val ++ "\""

printAttrs :: [TagAttr] -> String
printAttrs attrs = if null attrs then "" else " " ++ unwords (map printAttr attrs)

printHtmlTree :: Int -> HtmlTree -> IO ()
printHtmlTree level (Text content) = do 
    putStr $ indent level ++ "Text: " 
    LBS.putStr $ TE.encodeUtf8 $ T.pack $ unescape content 
    putStr "\n"
printHtmlTree level (HtmlTag tagType attrs children) =
    let openingTag = tagType ++ printAttrs attrs
    in do 
        putStrLn $ indent level ++ openingTag
        mapM_ (printHtmlTree (level + 1)) children

printHtml :: HtmlTree -> IO ()
printHtml = printHtmlTree 0

unescape :: String -> String
unescape [] = []
unescape ('\\':xs) = 
  let (numStr, rest) = span isDigit xs
      codePoint = case numStr of
                    "" -> '\\'
                    _  -> chr (fst (head (readDec numStr)))
  in codePoint : unescape rest
unescape (x:xs) = x : unescape xs

main :: IO ()
main = do
    putStr "Enter website URL (include http:// or https://): "
    hFlush stdout
    url <- getLine

    -- Fetch HTML content
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    let html = TE.decodeUtf8 $ LBS.toStrict $ S.getResponseBody response
    print html
    putStrLn "=== Tree ==="
    printHtml $ createTree $ show html

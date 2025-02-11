module Main where

{-# LANGUAGE OverloadedStrings #-}

-- Project modules
import HtmlTree
import TUI
import FetchHtml

-- Other imports
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    -- Prompt user for URL
    putStr "Enter website URL (include http:// or https://): "
    hFlush stdout
    url <- getLine

    -- Fetch Html
    html <- fetch url
        
    -- Print Web Page
    let tree = createTree html
    updateScreen tree



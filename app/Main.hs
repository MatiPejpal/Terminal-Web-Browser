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
    htmlContent <- fetch url
        
    -- Print Web Page
    let its = initialTuiState
    let tree = createTree htmlContent
    loadTUI
    appLoop its { html = Just tree }

appLoop :: TuiState -> IO ()
appLoop ts = do 
    updateScreen ts
    input <- getChar
    case input of 
        'k' -> appLoop ts { line = line ts+1 }
        'j' -> appLoop ts { line = line ts-1 }
        'q' -> clearTUI >> putStrLn "Thank you"
        _ -> appLoop ts

    



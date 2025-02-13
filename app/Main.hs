module Main where

{-# LANGUAGE OverloadedStrings #-}

-- Project modules
import TUI
import FetchHtml

-- Other imports
import System.IO

main :: IO ()
main = do
    -- Prompt user for URL
    putStr "Enter website URL (include http:// or https://): "
    hFlush stdout
    url <- getLine

    -- Fetch Html
    ts <- fetch url
        
    -- Print Web Page
    loadTUI
    appLoop ts

appLoop :: TuiState -> IO ()
appLoop ts = do 
    updateScreen ts
    input <- getChar
    case input of 
        'k' -> appLoop ts { line = line ts-1 }
        'j' -> appLoop ts { line = line ts+1 }
        'q' -> clearTUI >> putStrLn "Thank you"
        '>' -> do 
            clearTUI
            putChar '>'
            hFlush stdout
            url <- getLine
            newTs <- fetch url
            loadTUI
            appLoop newTs
        _ -> appLoop ts

    



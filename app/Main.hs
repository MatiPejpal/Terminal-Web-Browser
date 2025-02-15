module Main where

{-# LANGUAGE OverloadedStrings #-}

-- Project modules
import TUI
import FetchHtml

-- Other imports
import System.IO
import Text.Read (readMaybe)

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
    appLoop $ loadTUIState ts

appLoop :: TuiState -> IO ()
appLoop ts = do 
    updateScreen ts
    input <- getChar
    case input of 
        'k' -> appLoop ts { line = line ts-1 }
        'j' -> appLoop ts { line = line ts+1 }
        'q' -> clearTUI >> putStrLn "Thank you"
        '>' -> loadNewPage
        '<' -> linkJump ts
        _ -> appLoop ts

-- Ask user for new url, then fetch this url and display page
loadNewPage :: IO ()
loadNewPage =  do 
            clearTUI
            putChar '>'
            hFlush stdout
            url <- getLine
            newTs <- fetch url
            loadTUI
            appLoop $ loadTUIState newTs

linkJump :: TuiState -> IO ()
linkJump ts =  do 
            clearTUI
            putChar '<'
            hFlush stdout
            num <- getLine
            case readMaybe num of
                Nothing -> do 
                    loadTUI
                    appLoop ts
                Just n -> case getLink n ts of
                    Nothing -> appLoop ts
                    Just href -> do
                        newTs <- fetch href
                        loadTUI
                        appLoop $ loadTUIState newTs




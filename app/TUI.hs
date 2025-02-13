module TUI where

-- Text encoding
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS
import System.IO

-- Proejct modules
import HtmlTree

-- Other imports
import System.Console.ANSI 
import System.IO (hSetBuffering, BufferMode (NoBuffering), stdout)

-- Define TUIStatate
data TuiState = TuiState {
    html :: Maybe HtmlTree,
    layout :: Maybe String,
    line :: Int,
    mode :: Int,
    command :: Maybe String
    }

initialTuiState :: TuiState
initialTuiState = TuiState Nothing Nothing 0 0 Nothing

loadTUI :: IO ()
loadTUI = do
    clearScreen
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor

clearTUI :: IO ()
clearTUI = do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    showCursor

updateScreen :: TuiState -> IO ()
updateScreen state = case html state of
    Nothing -> putStrLn "No website fetched."
    Just tree -> do
        clearScreen
        setCursorPosition 0 0
        let tag = findTag "main" tree 
        case tag of
            Just t -> mapM_ putStrLn (take 10 $ drop (line state) $ lines $ printTag t)
            Nothing -> let body = findTag "body" tree
                    in case body of
                        Just b -> putStrLn $ printTag b ++ ['\r']
                        Nothing -> putStrLn "===Error==="


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


-- Returns a String that is layout of the Body
printTag :: HtmlTree -> String
printTag (Text text) = text
printTag (HtmlTag tag attr children) = 
    let layout = concatMap printTag children
    in layout ++ appendix tag
    where 
    -- define tag behaviour
    appendix "p" = "\n\n"
    appendix "div" = "\n\n"
    appendix "h1" = "\n\n"
    appendix "h2" = "\n\n"
    appendix "h3" = "\n\n"
    appendix "h4" = "\n\n"
    appendix "h5" = "\n\n"
    appendix "h6" = "\n\n"
    appendix "br" = "\n\n"
    appendix _ = ""


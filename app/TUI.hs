module TUI where

-- Text encoding
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS

-- Proejct modules
import HtmlTree

-- Other imports
import Data.Char (chr, isDigit)
import Numeric (readDec)
import System.Console.ANSI 

updateScreen :: HtmlTree -> IO ()
updateScreen tree = do
    clearScreen
    setCursorPosition 0 0
    let tag = findTag "main" tree
    case tag of
        Just b -> do 
            putStrLn $ printTag b
--            printHtml b
        Nothing -> putStrLn "Error"


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
printTag (Text text) = text ++ " "
printTag (HtmlTag tag attr children) = 
    let layout = concatMap printTag children
    in layout ++ appendix tag
    where 
    -- define tag behaviour
    appendix "p" = "\n\n"
    appendix "div" = "\n\n"
    appendix "h1" = "\n\n"
    appendix "br" = "\n\n"
    appendix _ = ""


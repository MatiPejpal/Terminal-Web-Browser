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
            putStrLn $ printTag b ++ "\n\n===Tree==="
            printHtml b
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

-- Turns substrings of encoded characters onto actual characters
unescape :: String -> String
unescape [] = []
unescape ('\\':'n':xs) = unescape xs
unescape ('\\':xs) = 
  let (numStr, rest) = span isDigit xs
      codePoint = case numStr of
                    "" -> '\\'
                    _  -> chr (fst (head (readDec numStr)))
  in codePoint : unescape rest
unescape (x:xs) = x : unescape xs

-- Returns a String that is layout of the Body
printTag :: HtmlTree -> String
printTag (Text text) = unescape text
printTag (HtmlTag tag attr children) = 
    let layout = concatMap printTag children
    in if isBlockTag tag
       then layout ++ "\n\n"
       else layout
    where
    -- Define block-level tags that need line breaks
    isBlockTag "p" = True
    isBlockTag "div" = True
    isBlockTag "h1" = True
    isBlockTag "br" = True  -- Special case for line breaks
    isBlockTag _ = False


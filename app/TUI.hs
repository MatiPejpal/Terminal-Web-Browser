module TUI where

-- Text encoding
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as LBS

-- Proejct modules
import HtmlTree

-- Other imports
import System.Console.ANSI 
import System.IO (hSetBuffering, BufferMode (NoBuffering, LineBuffering), stdin, hSetEcho)

-- Define TUIStatate
data TuiState = TuiState {
    html :: Maybe HtmlTree,
    layout :: Maybe String,
    links :: [(Int, String)],
    line :: Int
    }

initialTuiState :: TuiState
initialTuiState = TuiState Nothing Nothing [] 0 

-- Load terminal settings
loadTUI :: IO ()
loadTUI = do
    clearScreen
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hideCursor

-- Load orginal terminal settings
clearTUI :: IO ()
clearTUI = do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    showCursor

-- Loads TuiState, based of html tree produces layout
loadTUIState :: TuiState -> TuiState
loadTUIState ts = 
    case html ts of
        Nothing -> ts
        Just tree -> do
            let tag = findTag "main" tree 
            case tag of
                Just t -> do 
                    let newTree = numberTreeLinks t
                    let newLinks = gatherLinks newTree
                    ts {layout = Just $ printTag newTree, links = newLinks}
                Nothing -> let body = findTag "body" tree
                        in case body of
                            Just b -> do 
                                let newTree = numberTreeLinks b
                                let newLinks = gatherLinks newTree
                                ts {layout = Just $ printTag newTree, links = newLinks}
                            Nothing -> ts { layout = Just "===Error==="}

-- Displays current Tuistate onto the terminal
updateScreen :: TuiState -> IO ()
updateScreen state = case layout state of
    Nothing -> putStrLn "No layout found"
    Just l -> do 
        clearScreen
        setCursorPosition 0 0
        mapM_ putStrLn (take 25 $ drop (line state) $ lines l)
               
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
    appendix "a" = "[" ++ show (extractLinkNum attr) ++ "]"
    appendix _ = ""

-- number each link in tree
numberTreeLinks :: HtmlTree -> HtmlTree
numberTreeLinks tree = fst $ numberLinks tree 1

numberLinks :: HtmlTree -> Int -> (HtmlTree, Int)
numberLinks (Text t) linkNum = (Text t, linkNum)
numberLinks htag@(HtmlTag tag attr children) linkNum = if tag == "a"
    then (HtmlTag tag (attr++[TagAttr "link-number" $ show linkNum]) children, linkNum + 1)
    else let 
        (c, l) = numberLinksAux children linkNum
        in (htag {htmlchildren = c}, l)

numberLinksAux :: [HtmlTree] -> Int -> ([HtmlTree], Int)
numberLinksAux [] linkNum = ([], linkNum)
numberLinksAux (x:xs) linkNum = let 
    (tree, ln) = numberLinks x linkNum
    (trees, lns) = numberLinksAux xs ln
    in (tree:trees, lns)

extractLinkNum :: [TagAttr] -> Int
extractLinkNum [] = -1
extractLinkNum (x:xs) = if attrName x == "link-number"
    then read (attrVal x) :: Int
    else extractLinkNum xs

-- Loads links into tui state
loadLinks :: TuiState -> TuiState
loadLinks ts = case html ts of 
    Just a -> ts { links = gatherLinks a }
    Nothing -> ts

gatherLinks :: HtmlTree -> [(Int, String)]
gatherLinks (Text _) = []
gatherLinks (HtmlTag tag attr children) = if tag == "a"
    then [(extractLinkNum attr, extractLinkHref attr)]
    else concatMap gatherLinks children

extractLinkHref :: [TagAttr] -> String
extractLinkHref [] = ""
extractLinkHref (x:xs) = if attrName x == "href"
    then attrVal x
    else extractLinkHref xs

 


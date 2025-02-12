module HtmlTree where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List.Split
import Control.Monad (msum)

data HtmlTree
    = Text { content :: String }
    | HtmlTag 
     { htmltagType :: String
     , htmlAttrs :: [TagAttr]
     , htmlchildren :: [HtmlTree]}
    deriving (Show, Eq)

data TagAttr = TagAttr
    { attrName :: String
    , attrVal :: String}
    deriving (Show, Eq)

-- takes html text, tagType accumulator
-- returns tuple of (tagType, remaining html text, whether SelfClosing)
parseTagType :: String -> String -> (String, String, Bool, [TagAttr])
parseTagType [] tagType = ([], tagType, True, [])
parseTagType (x:xs) tagType
    | x == '/' = 
        if head xs == '>' 
        then let
        (tag, attrs) = parseAttr tagType
        in (tag, tail xs, True, attrs)
        else parseTagType xs $ tagType ++ [x]
    | x == '>' = let
        (tag, attrs) = parseAttr tagType
        in (tag, xs, False, attrs)
    | otherwise = parseTagType xs $ tagType ++ [x]

-- takes whole tag
-- returns tagType, list of attributes
parseAttr :: String -> (String, [TagAttr])
parseAttr = (fst &&& parseAttrList . splitOn "\"" . snd) . break isSpace

parseAttrList :: [String] -> [TagAttr]
parseAttrList [] = []
parseAttrList [_] = []
parseAttrList (x:y:zs) = TagAttr (init $ removeFirstLast x) (init y) : parseAttrList zs

removeFirstLast :: String -> String
removeFirstLast = tail . init

-- takes html text, children accumulator, text accumulator
-- returns a tuple of (list of children, remaining html text)
parseText :: String -> [HtmlTree] -> String -> ([HtmlTree], String)
parseText [] children _ = (children, [])
parseText (x:xs) children text 
    | x == '<' = if head xs == '/'
        then (children ++ [Text $ removeWhiteSpace text | not (allSpaceUtf8 text)], removeTag xs)
        else let
            (child, remaining) = parseTag xs
            in parseText remaining (children++[Text $ removeWhiteSpace text, child]) ""
    | otherwise = parseText xs children (text++[x])

-- Checks whether String consists only of whiteSpace
allSpaceUtf8 :: String -> Bool
allSpaceUtf8 [] = True
allSpaceUtf8 ('\\':'n':zs) = allSpaceUtf8 zs
allSpaceUtf8 ('\\':'t':zs) = allSpaceUtf8 zs
allSpaceUtf8 (' ':zs) = allSpaceUtf8 zs
allSpaceUtf8 _ = False

-- removes white space in front and at the end of string
removeWhiteSpace :: String -> String
removeWhiteSpace s = let 
    frontDrop = dropWhile isSpace s
    backDrop = dropWhile isSpace (reverse frontDrop)
    in reverse backDrop

parseTag :: String -> (HtmlTree, String)
parseTag xs
    | head xs `elem` ['!', '/'] = parseHTML $ removeTag xs
    | otherwise = (HtmlTag tagType attrs children, remaining2)
  where
    (tagType, remaining, selfClose, attrs) = parseTagType xs []
    (children, remaining2) = if selfClose 
        then ([], remaining) 
        else case tagType of 
            "code" -> parseAsText remaining ""
            "script" -> parseAsText remaining ""
            _ ->  parseText remaining [] ""

-- parse as text
parseAsText :: String -> String -> ([HtmlTree], String)
parseAsText [] text = ([Text text], "")
parseAsText ('<':'/':zs) text = ([Text text], removeTag zs) 
parseAsText (x:xs) text = parseAsText xs (text ++ [x])

-- removes everything until end of tag
removeTag :: String -> String
removeTag = drop 1 . dropWhile (/= '>')

parseHTML :: String -> (HtmlTree, String)
parseHTML = parseTag . drop 1 . dropWhile (/= '<')

createTree :: String -> HtmlTree
createTree = fst . parseHTML . removeTag

-- Find and return tag of HtmlTree
findTag :: String -> HtmlTree -> Maybe HtmlTree
findTag _ (Text _) = Nothing
findTag tagName body@(HtmlTag tagType _ children)
    | tagType == tagName = Just body
    | otherwise = msum $ map (findTag tagName) children


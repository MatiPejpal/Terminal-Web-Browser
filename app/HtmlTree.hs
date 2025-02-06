module HtmlTree where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List.Split

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
parseAttrList (x:y:zs) = TagAttr (removeFirstLast x) y : parseAttrList zs

removeFirstLast :: String -> String
removeFirstLast = tail . init

-- takes html text, children accumulator, text accumulator
-- returns a tuple of (list of children, remaining html text)
parseText :: String -> [HtmlTree] -> String -> ([HtmlTree], String)
parseText [] children _ = (children, [])
parseText (x:xs) children text 
    | x == '<' = if head xs == '/'
        then (children ++ [Text text | not (all isSpace text)], removeTag xs)
        else let
            (child, remaining) = parseTag xs
            in parseText remaining (children++[child]) ""
    | otherwise = parseText xs children (text++[x])

parseTag :: String -> (HtmlTree, String)
parseTag xs
    | head xs `elem` ['!', '/'] = parseHTML $ removeTag xs
    | otherwise = (HtmlTag tagType attrs children, remaining2)
  where
    (tagType, remaining, selfClose, attrs) = parseTagType xs []
    (children, remaining2) = if selfClose then ([], remaining) else parseText remaining [] ""

-- removes everything until end of tag
removeTag :: String -> String
removeTag = drop 1 . dropWhile (/= '>')

parseHTML :: String -> (HtmlTree, String)
parseHTML = parseTag . drop 1 . dropWhile (/= '<')

createTree :: String -> HtmlTree
createTree = fst . parseHTML . removeTag


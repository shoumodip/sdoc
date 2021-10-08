module Parser
  (
    Lexeme(Normal, Header, Code, NList, BList)
  , Document

  , parseDocument
  , parseLines
  ) where

import Data.Char (isSpace)
import qualified Data.Text as T

splitApply :: ([a] -> b)   -- The head function
           -> ([a] -> [b]) -- The tail function
           -> (a -> Bool)  -- The predicate
           -> [a]          -- The items
           -> [b]          -- The results
splitApply h t p xs = h (takeWhile p xs) : t (dropWhile p xs)

data Lexeme = Normal [T.Text]
            | Header Int T.Text
            | Code [T.Text]
            | NList [T.Text]
            | BList [T.Text]
            deriving Show

type Document = [Lexeme]

isCode :: T.Text -> Bool
isCode = T.isPrefixOf "```"

isNList :: T.Text -> Bool
isNList = T.isPrefixOf "*"

isBList :: T.Text -> Bool
isBList = T.isPrefixOf "-"

isHeader :: T.Text -> Bool
isHeader = T.isPrefixOf "#"

isNormal :: T.Text -> Bool
isNormal = not . or . sequence [isCode, isNList, isBList, isHeader]

skipN :: Int -> T.Text -> T.Text
skipN n = T.strip . T.drop n

skip1 :: T.Text -> T.Text
skip1 = T.strip . T.drop 1

headerLevel :: T.Text -> Int
headerLevel = T.length . T.takeWhile (== '#')

parseLines :: [T.Text] -> Document
parseLines [] = []
parseLines ls
  | isCode l   = splitApply Code (parseLines . tail) (not . isCode) $ tail ls
  | isNList l  = splitApply (NList . map skip1) parseLines isNList ls
  | isBList l  = splitApply (BList . map skip1) parseLines isBList ls
  | isHeader l = Header h (skipN h l):parseLines (tail ls)
  | otherwise  = splitApply Normal parseLines isNormal ls
  where l = head ls
        h = headerLevel l

parseDocument :: T.Text -> Document
parseDocument = parseLines . T.lines

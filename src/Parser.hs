{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Char (isSpace)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

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

isCode :: T.Text -> Bool
isCode = T.isPrefixOf ">"

isNList :: T.Text -> Bool
isNList = T.isPrefixOf "#"

isBList :: T.Text -> Bool
isBList = T.isPrefixOf "-"

isHeader :: T.Text -> Bool
isHeader s
  | T.null s  = False
  | otherwise = T.head s == '*'

isNormal :: T.Text -> Bool
isNormal l = not $ isCode l || isNList l || isBList l || isHeader l

skipN :: Int -> T.Text -> T.Text
skipN n = T.strip . T.drop n

skip1 :: T.Text -> T.Text
skip1 = T.strip . T.drop 1

headerLevel :: T.Text -> Int
headerLevel = T.length . T.takeWhile (== '*')

type Document = [Lexeme]

parseDocument :: [T.Text] -> Document
parseDocument [] = []
parseDocument ls
  | isCode l   = splitApply (Code . map skip1) parseDocument isCode ls
  | isNList l  = splitApply (NList . map skip1) parseDocument isNList ls
  | isBList l  = splitApply (BList . map skip1) parseDocument isBList ls
  | isHeader l = Header h (skipN h l):parseDocument (tail ls)
  | otherwise  = splitApply Normal parseDocument isNormal ls
  where l = head ls
        h = headerLevel l

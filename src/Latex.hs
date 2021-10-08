module Latex (document) where

import qualified Data.Text as T
import qualified Parser    as P

wrapEnv :: T.Text -> T.Text -> T.Text
wrapEnv e b = "\\begin{" <> e <> "}\n" <> b <> "\\end{" <> e <> "}"

listItems :: T.Text -> [T.Text] -> T.Text
listItems o = wrapEnv o . T.unlines . map ("\\item " <>)

lexemeLatex :: P.Lexeme -> T.Text
lexemeLatex (P.Normal t)   = T.unlines t
lexemeLatex (P.Header n t) = "\\" <> T.concat (replicate (n - 1) "sub") <> "title{" <> t <> "}\n"
lexemeLatex (P.Code t)     = wrapEnv "verbatim" $ T.unlines t
lexemeLatex (P.NList t)    = listItems "enumerate" t
lexemeLatex (P.BList t)    = listItems "itemize" t

document :: P.Document -> T.Text
document = (<>) "\\documentclass{article}\n"
  . wrapEnv "document"
  . T.unlines
  . map lexemeLatex

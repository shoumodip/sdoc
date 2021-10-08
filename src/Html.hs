module Html (document) where

import qualified Data.Text as T
import qualified Parser    as P

wrapTag :: T.Text -> T.Text -> T.Text
wrapTag t b = "<" <> t <> ">" <> b <> "</" <> t <> ">"

listItems :: T.Text -> [T.Text] -> T.Text
listItems o = wrapTag o . T.unlines . map (wrapTag "li")

lexemeHtml :: P.Lexeme -> T.Text
lexemeHtml (P.Normal t)   = T.unlines t
lexemeHtml (P.Header n t) = wrapTag (T.pack $ "h" <> show n) t
lexemeHtml (P.Code t)     = wrapTag "pre" $ T.unlines t
lexemeHtml (P.NList t)    = listItems "ol" t
lexemeHtml (P.BList t)    = listItems "ul" t

document :: P.Document -> T.Text
document = wrapTag "head"
  . wrapTag "body"
  . T.unlines
  . map lexemeHtml

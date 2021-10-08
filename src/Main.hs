import System.Environment    (getArgs)
import System.FilePath.Posix (takeExtension)

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Parser       as P
import qualified Html         as H
import qualified Latex        as L

sdocConvert :: FilePath -> P.Document -> T.Text
sdocConvert f
  | ext == "html" = H.document
  | ext == "tex"  = L.document
  | otherwise     = error $ "undefined generator for file path: " <> f
  where ext = drop 1 $ takeExtension f

sdocHtml :: [FilePath] -> IO ()
sdocHtml (input:output:_) = T.readFile input >>=
  T.writeFile output . sdocConvert output . P.parseDocument

main :: IO ()
main = getArgs >>= sdocHtml

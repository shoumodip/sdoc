import System.Environment

import qualified Data.Text.IO as T
import qualified Html         as H
import qualified Parser       as P

sdocHtml :: [FilePath] -> IO ()
sdocHtml (input:output:_) = T.readFile input >>=
  T.writeFile output . H.document . P.parseDocument

main :: IO ()
main = getArgs >>= sdocHtml

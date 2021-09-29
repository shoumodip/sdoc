import Parser
import qualified Data.Text    as T
import qualified Data.Text.IO as T

sampleFile = "examples/sample.sdoc"

main :: IO ()
main = do
  contents <- T.lines <$> T.readFile sampleFile
  mapM_ print $ parseDocument contents

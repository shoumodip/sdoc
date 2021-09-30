import Parser
import qualified Data.Text.IO as T

sampleFile :: String
sampleFile = "examples/sample.md"

main :: IO ()
main = T.readFile sampleFile >>= mapM_ print . parseDocument

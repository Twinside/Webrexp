import System.Environment
import Text.XML.HXT.Parser.HtmlParsec

main :: IO ()
main = do
      (filename:_) <- getArgs
      file <- readFile filename
      print $ parseHtmlContent file


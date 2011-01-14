
import Network.HTTP
import Network.HTTP.Base

{-import Text.XML.HXT-}
{-import Text.XML.HXT.DOM.TypeDefs-}
import Text.XML.HXT.Parser.HtmlParsec

-- This file is to test binary file downloading
-- using bytestring. It used the http library
-- directly to find it.
url :: String
url = "http://twinside.free.fr/supaview/"

main :: IO ()
main = do
      rsp <- Network.HTTP.simpleHTTP (getRequest url)
      body <- getResponseBody rsp
      print $ parseHtmlContent body


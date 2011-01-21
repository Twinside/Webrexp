
import Network.HTTP
import Network.HTTP.Base
import Network.Browser

{-import Text.XML.HXT-}
{-import Text.XML.HXT.DOM.TypeDefs-}
import Text.XML.HXT.Parser.HtmlParsec

-- This file is to test binary file downloading
-- using bytestring. It used the http library
-- directly to find it.
url :: String
url = "http://www.google.com"

main :: IO ()
main = do
      (u, rsp) <- Network.Browser.browse $ do
            setAllowRedirects True
            request $ getRequest url
      putStrLn "-------------------------------------"
      putStrLn $ "URL=" ++ show u
      putStrLn "-------------------------------------"
      putStrLn $ show rsp
      putStrLn "-------------------------------------"
      print $ parseHtmlContent $ rspBody rsp


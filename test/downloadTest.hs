import Network.HTTP
import Network.HTTP.Base
import Network.URI( parseURI )
import qualified Data.ByteString.Lazy as B

-- This file is to test binary file downloading
-- using bytestring. It used the http library
-- directly to find it.
url :: String
url = "http://www.vim.org/images/vim_header.gif"

req :: String -> Request B.ByteString
req s = 
    case parseURI s of
         Nothing -> error "ill formed"
         Just s -> mkRequest GET s

main :: IO ()
main = do
      rsp <- Network.HTTP.simpleHTTP (req url)
      body <- getResponseBody rsp
      B.writeFile "out.gif" body


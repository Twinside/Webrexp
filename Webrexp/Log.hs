
module Webrexp.Log ( 
      errorLog
    , infoLog
    , networkInfo
    , networkError
    , debugLog
    , textOutput 
    ) where

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Webrexp.WebContext

errorLog :: String -> WebCrawler node ()
errorLog = liftIO . hPutStr stderr

infoLog :: String -> WebCrawler node ()
infoLog = liftIO . putStrLn

networkInfo :: String -> WebCrawler node ()
networkInfo = liftIO . putStrLn

networkError :: String -> WebCrawler node ()
networkError = liftIO . hPutStr stderr

debugLog :: String -> WebCrawler node ()
debugLog str = do
    verb <- isVerbose
    when (verb) (liftIO $ putStrLn str)

textOutput :: String -> WebCrawler node ()
textOutput = liftIO . putStrLn


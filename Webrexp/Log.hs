
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

errorLog :: String -> WebCrawler node rezPath ()
errorLog = liftIO . hPutStr stderr

infoLog :: String -> WebCrawler node rezPath ()
infoLog = liftIO . putStrLn

networkInfo :: String -> WebCrawler node rezPath ()
networkInfo = liftIO . putStrLn

networkError :: String -> WebCrawler node rezPath ()
networkError = liftIO . hPutStr stderr

debugLog :: String -> WebCrawler node rezPath ()
debugLog str = do
    verb <- isVerbose
    when (verb) (liftIO $ putStrLn str)


textOutput :: String -> WebCrawler node rezPath ()
textOutput str = do
    handle <- getOutput
    liftIO $ hPutStrLn handle str



-- | Implementation of the (rather basic) logging facility. 
--
-- Avoid using 'putStrLn' or 'putStr' in the project, favor
-- using this module.
module Text.Webrexp.Log ( 
      debugLog
    , textOutput 
    ) where

import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Text.Webrexp.WebContext

-- | Debugging function, only displayed in verbose
-- logging mode.
debugLog :: String -> WebCrawler node rezPath ()
debugLog str = do
    verb <- isVerbose
    when verb (liftIO $ putStrLn str)


-- | If a webrexp output some text, it must go through
-- this function. It ensure the writting in the correct
-- file.
textOutput :: String -> WebCrawler node rezPath ()
textOutput str = do
    handle <- getOutput
    liftIO $ hPutStr handle str



-- | Implementation of the (rather basic) logging facility. 
--
-- Avoid using 'putStrLn' or 'putStr' in the project, favor
-- using this module.
module Text.Webrexp.Log ( 
      debugLog
    , textOutput 
    ) where

import Text.Webrexp.WebContext( debugLog, textOutput )


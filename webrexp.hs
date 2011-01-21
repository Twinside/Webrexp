
-- | Generic module for using Webrexp as a user.
module Webrexp ( webRexpParser, evalWebRexp ) where

import Webrexp.Parser( webRexpParser )
-- import qualified Webrexp.Eval as E

-- we need the instance
import Webrexp.HxtNode()

evalWebRexp :: String -> IO Bool
evalWebRexp _ = return False


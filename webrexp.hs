-- | Generic module for using Webrexp as a user.
module Webrexp 
    ( module Webrexp.Eval
    , module Webrexp.Parser
    )
    where

import Webrexp.Eval( evalWebRexp )
import Webrexp.Parser( webRexpParser )


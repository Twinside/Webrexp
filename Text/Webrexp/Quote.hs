{-# LANGUAGE FlexibleInstances #-}
module Text.Webrexp.Quote ( webrexpParse
                          , webrexpCompile ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Webrexp
import Text.Webrexp.WebRexpAutomata 

-- | QuasiQuotation to transform a webrexp to
-- it's AST representation, resulting type is :: Webrexp.
-- You can use it the following way :
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Text.Webrexp.Quote
-- >
-- > [webrexpParse| some webrexp [.] |]
--
webrexpParse :: QuasiQuoter
webrexpParse = QuasiQuoter 
        { quoteExp = parser
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }

parser :: String -> Q Exp
parser s = case parseWebRexp s of
    Nothing -> fail "Invalid webrexp syntax"
    Just w -> lift w

-- | Quasi quote to transform a webrexp into it's compiled representation.
-- You can use it the following way :
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Text.Webrexp.Quote
-- >
-- > [webrexpCompile| some webrexp [.] |]
--
webrexpCompile :: QuasiQuoter
webrexpCompile = QuasiQuoter 
        { quoteExp = compiler
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }

-- | Transform a webrexp to it's \'compiled\'
-- form, resulting type is :: Automata
compiler :: String -> Q Exp
compiler s = case parseWebRexp s of
    Nothing -> fail "Invalid webrexp syntax"
    Just w -> lift $ buildAutomata w


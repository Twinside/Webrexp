{-# LANGUAGE FlexibleInstances #-}
module Text.Webrexp.Quote ( webrexpParse
                          , webrexpCompile ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Text.Webrexp
import Text.Webrexp.WebRexpAutomata 

-- | QuasiQuotation to transform a wabrexp to
-- it's AST representation, resulting type is
-- :: WebRexp 
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


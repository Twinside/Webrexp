{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.Webrexp.Eval.ActionFunc( ActionValue(..)
                              , ActionFunc
                              , ActionFuncM
                              , toNum 
                              , toString
                              , funToString 
                              , trimString
                              , formatString 
                              , format
                              , substituteFunc
                              , funcSysCall
                              ) where

import Control.Applicative
import Data.Char
import System.Process
import System.Exit

import Debug.Trace

import Text.Webrexp.IOMock
import Text.Webrexp.WebContext

-- | Data used for the evaluation of actions. Represent the
-- whole set of representable data at runtime.
data ActionValue =
      AInt    Int
    | ABool   Bool
    | AString String
    | ATypeError
    deriving (Eq, Show)

-- | Type used to describe evaluator for function inside
-- webrexp actions.
type ActionFunc node rezPath
     = [ActionValue]                 -- ^ Argument list
     -> Maybe (EvalState node rezPath) -- ^ Pipeline argument
     -> (ActionValue, Maybe (EvalState node rezPath)) -- ^ Result

type ActionFuncM array node rezPath m
     = [ActionValue]                 -- ^ Argument list
     -> Maybe (EvalState node rezPath) -- ^ Pipeline argument
     -> WebContextT array node rezPath m
                    (ActionValue, Maybe (EvalState node rezPath)) -- ^ Result

-- | Typecast operation, from :
-- - string to int
-- - Bool to int
toNum :: ActionFunc node rezPath
toNum [AString t] a | all isDigit t = (AInt $ read t, a)
toNum [ABool True] a = (AInt 1, a)
toNum [ABool False] a = (AInt 0, a)
toNum [v@(AInt _)] a = (v, a)
toNum _ a = (ATypeError, a)

-- | Convert any value to string
toString :: ActionValue -> String
toString (AString v) = v
toString (ABool True) = "true"
toString (ABool False) = "false"
toString (AInt i) = show i
toString ATypeError = "ATypeError"

funToString :: ActionFunc node rezPath
funToString [ATypeError] a = trace "FUCK" (ATypeError, a)
funToString [v] a = (AString $ toString v, a)
funToString b a = trace (show b) (ATypeError, a)


-- | Remove blank space before and after a string
trimString :: ActionFunc node rezPath
trimString [AString s] a = (AString $ trimm s, a)
    where trimm = reverse . trimBegin . reverse . trimBegin
          trimBegin = dropWhile (\c -> c `elem` " \t")

trimString _ a = (ATypeError, a)

-- | This function take a string as first parameter
-- (the template string) and a list of string to be
-- inserted at some points.
--
-- The format string is made up of some tagged indices,
-- for example \'{0}\' reference the first inserted content
-- and \'{2}\' the third one. the \'}\' character can be
-- escapped by prefixing it by a \'\\\'
--
-- @
--    format \"da {0} bu {1} \\\\{0} do {1}\" [\"head\", \"second\"]
--    -> Just \"da head bu second {0} do second\"
-- @
--
-- It work as intented, there is no syntax error in the formated
-- string, and all indices are in bound.
--
-- @
--    format \"da {0} bu {1} \\\\{0} do {2}\" [\"head\", \"second\"]
--    -> 'Nothing'
-- @
--
-- the \'2\' index is out of bound, so the function return
-- 'Nothing'
--
-- @
--    format \"da {0} bu {1} \\\\{0} do {1a}\" [\"head\", \"second\"]
--    -> 'Nothing'
-- @
--
-- 'Nothing' is returned because \'1a\' is not a valid index.
format :: String        -- ^ Template string
       -> [String]      -- ^ Inserted content
       -> Maybe String  -- ^ The formated string
format [] _ = Just ""
format ('\\':'{':xs) els = ('{' :) <$> format xs els
format ('{':xs) els = 
    let maxIndex = length els
    in case span isDigit xs of
        ([],  _) -> Nothing
        (_ , []) -> Nothing
        (n, '}':rest) -> let idx = read n in
            if idx >= maxIndex
                then Nothing
                else ((els !! idx) ++) <$> format rest els
        _ -> Nothing
format (x:xs) els = (x:) <$> format xs els

-- | Format a string given a list of action
-- values, if the first parameter is not a
-- string, return a type error.
formatString :: ActionFunc node rezPath
formatString (AString s : rest) a =
    (maybe ATypeError AString . format s $ map toString rest, a)
formatString _ a = (ATypeError, a)

-- | Given a prefix and a list, return the rest
-- of the list 
dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix []      lst = Just lst
dropPrefix (x:xs) (y:ys)
    | x == y = dropPrefix xs ys
    | otherwise = Nothing
dropPrefix _      [] = Nothing

-- | Replace globally (for each repeatition)
-- a sublist by another one in a give list
substitute :: (Eq a)
           => [a]    -- ^ The substituted list
           -> [a]    -- ^ The replaced sublist
           -> [a]    -- ^ The replacement
           -> [a]
substitute [] _ _ = []
substitute lst@(_:xs) what by
    | Just rest <- dropPrefix what lst =
        by ++ substitute rest what by
    | otherwise = substitute xs what by

substituteFunc :: ActionFunc node rezPath
substituteFunc [AString s, AString what, AString by] e =
    (AString $ substitute s what by, e)
substituteFunc _ e = (ATypeError, e)

funcSysCall :: (IOMockable (WebContextT array node rezPath m), Monad m)
            => ActionFuncM array node rezPath m
funcSysCall [AString s] e = do
    code <- performIO $ system s
    case code of
         Just ExitSuccess -> return (ABool True, e)
         _ -> return (ABool False, e)
funcSysCall _ e = return (ATypeError, e)


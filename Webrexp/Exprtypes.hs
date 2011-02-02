-- | Datatypes used to describe webrexps, and some helper functions.
module Webrexp.Exprtypes
    ( 
    -- * Types
      WebRef (..)
    , NodeRange (..)
    , Op (..)
    , ActionExpr (..)
    , WebRexp (..)
    -- * Functions
    , simplifyNodeRanges 
    , foldWebRexp
    , setUniqueIndices 
    , prettyShowWebRef
    ) where

import Data.List( sort, mapAccumR )

-- | represent an element
data WebRef =
    -- | ... Search for a named element.
      Elem String
    -- | ... . ...  Check the value of the \'class\' attribute
    | OfClass WebRef String
    -- | \@... Check for the presence of an attribute
    | Attrib WebRef String
    -- | #...  Check the value of the \'id\' attribute
    | OfName WebRef String
    deriving (Eq, Show)

-- | Ranges to be able to filter nodes by position.
data NodeRange =
    -- | ...
      Index Int
    -- | min-max
    | Interval Int Int
    deriving (Eq, Show)

instance Ord NodeRange where
    compare (Index a) (Index b) = compare a b
    compare (Index a) (Interval b c)
        | a  < b = LT
        | a  > c = GT
        -- index is within the interval, put interval
        -- before
        | otherwise = GT
    compare (Interval a _) (Index c)
        | a < c = LT
        | a > c = GT
        | otherwise = LT -- favor interval before
    compare (Interval a b) (Interval c d) =
        case compare a c of
             EQ -> compare b d
             e -> e

simplifySortedNodeRanges :: [NodeRange] -> [NodeRange]
simplifySortedNodeRanges [] = []
simplifySortedNodeRanges [a] = [a]
simplifySortedNodeRanges (i@(Interval a b):idx@(Index c):xs)
    | a <= c && c <= b = simplifySortedNodeRanges (i:xs)
    | otherwise = i : simplifySortedNodeRanges (idx:xs)
simplifySortedNodeRanges (i1@(Index a):i2@(Index b):xs)
    | a == b = simplifySortedNodeRanges (i1:xs)
    | otherwise = i1 : simplifySortedNodeRanges (i2:xs)
simplifySortedNodeRanges (i1@(Index _):i2@(Interval _ _):xs) =
    i1 : simplifySortedNodeRanges (i2:xs)
simplifySortedNodeRanges (i1@(Interval a b):i2@(Interval c d):xs)
    | a <= c && c <= b = -- there is intersection, pick the union
        simplifySortedNodeRanges $ Interval a (max b d) : xs
    | otherwise = i1 : simplifySortedNodeRanges (i2:xs)

-- | This function is an helper function to simplify
-- the handling the node range. After simplification,
-- the ranges are sorted in ascending order and no
-- node range overlap.
simplifyNodeRanges :: [NodeRange] -> [NodeRange]
simplifyNodeRanges = simplifySortedNodeRanges . sort . map rangeRearranger
    where rangeRearranger i@(Index _) = i
          rangeRearranger i@(Interval a b)
            | a == b = Index a
            | a > b = Interval b a
            | otherwise = i

-- | Definitions of the operators available in
-- the actions of the webrexp.
data Op =
      OpAdd -- ^ '+'
    | OpSub -- ^ '-'
    | OpMul -- ^ '*'
    | OpDiv -- ^ 'div'
    | OpLt  -- ^ '<'
    | OpLe  -- ^ '<='
    | OpGt  -- ^ '>'
    | OpGe  -- ^ '>='
    | OpEq  -- ^ \'=\' in webrexp ('==' in Haskell)
    | OpNe  -- ^ \'!=\' ('/=' in Haskell)
    | OpAnd -- ^ \'&\' ('&&' in Haksell)
    | OpOr  -- ^ \'|\' ('||' in Haskell)
    deriving (Eq, Show)

-- | Represent an action Each production
-- of the grammar more or less map to a
-- data constructor of this type.
data ActionExpr =
    -- | { ... ; ... ; ... ; ... }
    -- A list of action to execute, each
    -- one must return a 'valid' value to
    -- continue the evaluation
      ActionExprs [ActionExpr]
    -- | Basic binary opertor application
    | BinOp Op ActionExpr ActionExpr

    -- | Find a value of a given attribute for
    -- the current element.
    | ARef String

    -- | An integer constant.
    | CstI Int

    -- | A string constant
    | CstS String

    -- | \'$\'... operator
    -- Used to put the action value back into
    -- the evaluation pipeline.
    | NodeReplace ActionExpr

    -- | the '.' action. Dump the content of
    -- the current element.
    | OutputAction
    deriving (Eq, Show)


-- | Type representation of web-regexp,
-- main type.
data WebRexp =
    -- | ( ... ; ... ; ... )
      Branch [WebRexp]
    -- | ... ... (each action followed, no rollback)
    | List [WebRexp]
    -- | ... *
    | Star WebRexp
    -- | ... +
    | Plus WebRexp
    -- | \'|\' Represent two alternative path, if
    -- the first fail, the second one is taken
    | Alternative WebRexp WebRexp
    -- | \'!\'
    -- Possess an unique index to differentiate all the differents
    -- uniques. Negative value are considered invalid, all positive or
    -- null one are accepted.
    | Unique Int
    -- | \"...\" A string constant in the source expression.
    | Str String
    -- | \"{ ... }\"
    | Action ActionExpr
    -- | \'[ ... ]\' Filtering Range
    | Range [NodeRange]
    -- | every tag/class name
    | Ref WebRef

    -- | \'>\' operator in the language, used
    -- to follow hyper link
    | DiggLink

    -- | \'/\' operator in the language, used
    -- to select the next sibling node.
    | NextSibling

    -- | \'^\' operator in the language, used
    -- to select the previous sibling node.
    | PreviousSibling

    -- | \'<\' operator in the language. 
    -- Select the parent node
    | Parent
    deriving (Eq, Show)

-- | This function permit the rewriting of a wabrexp in a depth-first
-- fashion while carying out an accumulator.
foldWebRexp :: (a -> WebRexp -> (a, WebRexp)) -> a -> WebRexp -> (a, WebRexp)
foldWebRexp f acc (Branch subs) = f acc' $ Branch subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (List subs) = f acc' $ List subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (Star sub) = f acc' $ Star sub'
    where (acc', sub') = foldWebRexp f acc sub
foldWebRexp f acc (Plus sub) = f acc' $ Plus sub'
    where (acc', sub') = foldWebRexp f acc sub
foldWebRexp f acc e = f acc e

-- | Set the index for every unique, return the
-- new webrexp and the count of unique element
setUniqueIndices :: WebRexp -> (Int, WebRexp)
setUniqueIndices expr = foldWebRexp uniqueCounter 0 expr
    where uniqueCounter acc (Unique _) = (acc + 1, Unique acc)
          uniqueCounter acc e = (acc, e)


-- | Pretty printing for 'WebRef'. It's should be reparsable
-- by the WebRexp parser.
prettyShowWebRef :: WebRef -> String
prettyShowWebRef (Elem s) = s
prettyShowWebRef (OfClass r s) = prettyShowWebRef r ++ "." ++ s
prettyShowWebRef (Attrib r s) = prettyShowWebRef r ++ "@" ++ s
prettyShowWebRef (OfName r s) = prettyShowWebRef r ++ "#" ++ s

